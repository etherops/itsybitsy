# Copyright # Copyright 2020 Life360, Inc
# SPDX-License-Identifier: Apache-2.0

__version__ = "1.1.0"

import asyncio
import getpass
import logging
import os
import signal
import sys
from contextlib import contextmanager
from termcolor import colored
from typing import Dict

from . import charlotte, charlotte_web, cli_args, constants, crawl, logs, node, plugin_core, providers, renderers
from .plugins import render_json, render_ascii


# python3 check
REQUIRED_PYTHON_VERSION = (3, 8)
def tuple_join(the_tuple):
    return '.'.join(str(i) for i in the_tuple)


if sys.version_info[0] < REQUIRED_PYTHON_VERSION[0] or sys.version_info[1] < REQUIRED_PYTHON_VERSION[1]:
    print(f"Python version {tuple_join(sys.version_info[:2])} detected. This script requires Python version >= "
          f"{tuple_join(REQUIRED_PYTHON_VERSION)} available at `/usr/bin/env python3`")
    sys.exit(1)

# catch ctrl-c
signal.signal(signal.SIGINT, lambda x, y: sys.exit(0))


def main():
    print(f"Hello, {getpass.getuser()}", file=sys.stderr)
    plugin_core.import_plugin_classes()
    _parse_builtin_args()
    _set_debug_level()
    charlotte.init()
    _create_outputs_directory_if_absent()
    _cli_command().exec()
    print(f"\nGoodbye, {getpass.getuser()}\n", file=sys.stderr)


def _parse_builtin_args():
    try:
        with _suppress_console_out():
            constants.ARGS, _ = cli_args.parse_args(renderers.get_renderer_refs())
    except SystemExit:
        # this is done in order to display custom plugin level arguments in --help script output
        providers.parse_provider_args(cli_args.spider_subparser)
        renderers.parse_renderer_args(cli_args.spider_subparser)
        renderers.parse_renderer_args(cli_args.render_subparser)
        cli_args.argparser.parse_known_args()


@contextmanager
def _suppress_console_out():
    with open(os.devnull, "w") as devnull:
        old_stdout = sys.stdout
        old_stderr = sys.stderr
        sys.stdout = devnull
        sys.stderr = devnull
        try:
            yield
        finally:
            sys.stdout = old_stdout
            sys.stderr = old_stderr


class Command:
    def exec(self):
        self._initialize_plugins()
        constants.ARGS = cli_args.argparser.parse_args()
        tree = self._generate_tree()
        _render(tree)

    def _initialize_plugins(self):
        raise NotImplementedError('Plugin initialization not implemented')

    def _generate_tree(self) -> Dict[str, node.Node]:
        raise NotImplementedError('Tree generation not implemented')


class RenderCommand(Command):
    def _initialize_plugins(self):
        _initialize_renderers()

    def _generate_tree(self) -> Dict[str, node.Node]:
        if not constants.ARGS.output:
            constants.ARGS.output = ['ascii']
        return render_json.load(constants.ARGS.json_file or constants.LASTRUN_FILE)


class SpiderCommand(Command):
    def _initialize_plugins(self):
        _initialize_renderers(True)
        _initialize_providers()

    def _generate_tree(self) -> Dict[str, node.Node]:
        tree = asyncio.get_event_loop().run_until_complete(_crawl_water_spout())
        render_json.dump(tree, constants.LASTRUN_FILE)
        return tree


def _cli_command() -> Command:
    if cli_args.command_render == constants.ARGS.command:
        return RenderCommand()
    elif cli_args.command_spider == constants.ARGS.command:
        return SpiderCommand()
    else:
        print(colored(f"Invalid command: {constants.ARGS.command}.  Please file bug with maintainer.", 'red'))
        sys.exit(1)


async def _crawl_water_spout():
    tree = _parse_seed_tree()
    await _crawl_and_render_to_stderr_unless_quiet_is_specified(tree)
    return tree


async def _crawl_and_render_to_stderr_unless_quiet_is_specified(tree: Dict[str, node.Node]):
    outfile = open(os.devnull, 'w') if constants.ARGS.quiet else sys.stderr
    crawl_tasks = [
        crawl.crawl(tree, []),
        render_ascii.render_tree(tree, [], out=outfile, print_slowly_for_humans=True)
    ]
    await asyncio.gather(*crawl_tasks)


def _parse_seed_tree() -> Dict[str, node.Node]:
    return {
        f"SEED:{address}":
            node.Node(
                crawl_strategy=charlotte.SEED_CRAWL_STRATEGY,
                protocol=charlotte_web.PROTOCOL_SEED,
                protocol_mux='seed',
                provider=provider,
                containerized=providers.get_provider_by_ref(provider).is_container_platform(),
                from_hint=False,
                address=address
            )
        for provider, address in [seed.split(':') for seed in constants.ARGS.seeds]
    }


def _initialize_renderers(parse_spider_command_args: bool = False):
    renderers.parse_renderer_args(cli_args.render_subparser)
    if parse_spider_command_args:
        renderers.parse_renderer_args(cli_args.spider_subparser)
    constants.ARGS, _ = cli_args.argparser.parse_known_args()
    renderers.register_renderers()


def _initialize_providers():
    providers.parse_provider_args(cli_args.spider_subparser)
    constants.ARGS, _ = cli_args.argparser.parse_known_args()
    providers.register_providers()


def _create_outputs_directory_if_absent():
    if not os.path.exists(constants.OUTPUTS_DIR):
        os.makedirs(constants.OUTPUTS_DIR)


def _render(tree: Dict[str, node.Node]) -> None:
    if not constants.ARGS.output:
        return
    for renderer_ref in constants.ARGS.output:
        renderer = renderers.get_renderer_by_ref(renderer_ref)
        renderer.render(tree)


def _set_debug_level():
    if constants.ARGS.debug:
        logs.logger.setLevel(logging.DEBUG)
