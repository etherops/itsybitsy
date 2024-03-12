# Copyright # Copyright 2020 Life360, Inc
# SPDX-License-Identifier: Apache-2.0

"""
Renders node output to the screen
"""
import asyncio
import sys
from collections import namedtuple
from dataclasses import asdict
from string import Template
from termcolor import colored
from termcolor.termcolor import Color
from typing import List, Dict

from itsybitsy import constants, logs, renderers
from itsybitsy.node import Node


def colored_force(text: str, color: Color) -> str:
    """We are only doing this because tests fail if we don't force_color=True
    due to a tty check in termcolor, and that we haven't figured out the secret
    sauce to monkeypatching sys.stdin.istty() via fixture in tests where there
    is no real tty!"""
    return colored(text, color, force_color=True)


class RendererAscii(renderers.RendererInterface):
    @staticmethod
    def ref() -> str:
        return 'ascii'

    def render(self, tree: Dict[str, Node]):
        asyncio.get_event_loop().run_until_complete(render_tree(tree, [], sys.stdout))

    @staticmethod
    def register_cli_args(argparser: renderers.RendererArgParser):
        argparser.add_argument('--verbose', action='store_true', help='Verbose mode for ascii renderer')


live_render_lock = asyncio.Lock()
Ancestor = namedtuple('Ancestor', 'last_sibling spacing')

# errors/warnings
error_messages = {
    'NULL_ADDRESS': Template("service '$service_name' detected but an instance address is not available to crawl!"),
    'TIMEOUT': Template("SSH timeout connecting to service:'$service_name' at address: '$address'"),
    'AWS_LOOKUP_FAILED': Template("AWS name lookup failed for :'$service_name' at address: '$address'")
}
warning_messages = {
    'CYCLE': Template("service '$service_name' discovered as a parent of itself!"),
    'DEFUNCT': Template("service '$service_name' configuration present on parent, but it not in use!")
}
_sleep_for_humans_seconds: float = .01
_sleep_for_humans_counter = 0


async def render_tree(nodes: Dict[str, Node], parents: List[Ancestor], out=sys.stderr,
                      print_slowly_for_humans: bool = False) -> None:
    """
    Render tree during live crawling of the graph.

    For better or for worse... it relies upon the node items it is iterating through
    being mutated elsewhere in the execution of this program.  This is how we achieve
    asynchronous crawling, yet synchronous rendering!

    It waits for 'children' to be added to the node before rendering, indicating
    that requisite stages of the crawling process are complete

    :param nodes: - the tree (or subset thereof) of Node() objects to render
    :param parents: - list of ancestors for the tree - used for rendering depth/context
    :param out: - a file-like object (stream) where to print, defaults to stderr
    :param print_slowly_for_humans: - if we are rendering an already crawled tree, print slowly so humans can see
    :return: None
    """
    await _wait_for_service_names(nodes, len(parents))
    nodes_merged = _merge_nodes_by_service_name(renderers.merge_hints(nodes))

    depth = len(parents)
    nodes_to_render = nodes_merged.copy()
    while len(nodes_to_render) > 0:
        for node_ref in list(sorted(nodes_to_render)):  # list b/c we cannot mutate a dict as we loop
            node = nodes_merged[node_ref]
            if node.warnings.get('DEFUNCT') and constants.ARGS.hide_defunct:
                nodes_to_render.pop(node_ref)
                continue

            if node.crawl_complete(depth):
                # this sleep allows human eyes to comprehend output
                if print_slowly_for_humans:
                    await asyncio.sleep(_get_sleep_for_humans_seconds())
                is_last_sibling = 1 == len(nodes_to_render)

                # set up recursive children's parent back-reference
                childrens_ancestors = parents.copy()  # copy isolates branch parents
                childrens_ancestors.append(Ancestor(is_last_sibling, len(node.protocol.ref)))

                # print prefixes
                print_prefix = _render_node_display_prefix(parents)
                error_print_prefix = _render_node_display_prefix(childrens_ancestors)

                # render
                _render_node(node, depth, print_prefix, is_last_sibling, out)
                if constants.ARGS.render_ascii_verbose:
                    _render_node_errs_warns(node, error_print_prefix, out)

                nodes_to_render.pop(node_ref)

                if len(childrens_ancestors) <= constants.ARGS.max_depth and node.children:
                    await render_tree(node.children, childrens_ancestors, out, print_slowly_for_humans)

        # this sleep prevents CPU hoarding
        if (len(nodes_to_render)) > 0:
            await asyncio.sleep(.1)

        if constants.ARGS.debug:
            logs.logger.debug(f"Waiting for crawl to complete for {str(len(nodes_to_render))} "
                              f"nodes at depth {str(len(parents))}...")
            logs.logger.debug(nodes_to_render)
            await asyncio.sleep(5)


def _get_sleep_for_humans_seconds() -> float:
    global _sleep_for_humans_counter, _sleep_for_humans_seconds
    if _sleep_for_humans_counter == 100:
        _sleep_for_humans_seconds /= 1.2
        _sleep_for_humans_counter = 0
    _sleep_for_humans_counter += 1
    return _sleep_for_humans_seconds


def _render_node_display_prefix(parents: List[Ancestor]) -> str:
    """
    Create print prefix which will render the node at the correct indentation on screen

    :param parents: - list of Ancestor objects - informs indentations/formatting
    :return: - the formatted print prefix
    """
    print_prefix = ''
    for i, parent in enumerate(parents):
        if 0 == i:
            print_prefix += ' '
            continue
        parent_branch = ' ' if parent.last_sibling else '|'
        print_prefix += f"{parent_branch}       " + ' ' * parent.spacing

    return print_prefix


def _render_node(node: Node, depth: int, prefix: str, is_last_sibling: bool, out):
    """
    This function does the main work for rendering the node to the screen.

    It formats the various outputs and ultimately prints the node and any associated crawling
    errors.

    :param node:
    :param depth:
    :param prefix:
    :param is_last_sibling:
    :param out: - file-like object to print to (sys.stderr, sys.stdout)
    :return: None
    """
    # service name
    service_name = node.service_name or 'UNKNOWN'

    # terminus
    if node.warnings.get('DEFUNCT'):
        terminus = 'x'
    elif node.errors:
        terminus = '?'
    else:
        terminus = '>'

    # branch
    branch = ''
    if depth > 0:
        bud = '{}'.format('└' if is_last_sibling else '|') if not node.warnings.get('CYCLE') else '<'
        branch += f"{bud}--{node.protocol.ref}--{terminus} "

    # hint display
    info = colored_force('{INFO:FROM_HINT} ', "cyan") if node.from_hint else ''

    # concise warning display
    concise_warnings = ''
    if not constants.ARGS.render_ascii_verbose and node.warnings:
        concise_warnings = colored_force('{WARN:' + '|'.join(node.warnings) + '} ', "yellow")

    # concise error display
    concise_errors = ''
    if not constants.ARGS.render_ascii_verbose and node.errors:
        concise_errors = colored_force('{ERR:' + '|'.join(node.errors) + '} ', "red")

    # newline for SEED nodes
    if 0 == depth:
        print("")

    # protocol mux
    protocol_mux = f"port:{node.protocol_mux}" if node.protocol.blocking and depth > 0 else node.protocol_mux

    # print node
    address = f" ({node.address})" if constants.ARGS.render_ascii_verbose else ''
    line = f"{prefix}{branch}{info}{concise_warnings}{concise_errors}{service_name} [{protocol_mux}]{address}"
    print(line, file=out)


def _render_node_errs_warns(node: Node, error_prefix: str, out):
    """
    Render errors and warnings for the node on a new line

    :param node:
    :param error_prefix:
    :param out: - file-like object to print to (sys.stderr, sys.stdout)
    :return:
    """
    # errors/warnings
    error_messages = {
        'CONNECT_SKIPPED': f"service detected on {node.protocol.ref}:{node.protocol_mux}, however name discovery"
                           f"and crawling skipped by configuration!",
        'NULL_ADDRESS': f"service '{node.service_name}' detected but an instance address is not available to crawl!",
        'TIMEOUT': f"SSH timeout connecting to service:'{node.service_name}' at address: '{node.address}'",
        'AWS_LOOKUP_FAILED': f"AWS name lookup failed for :'{_synthesize_node_ref(node, 'UNKNOWN')}'"
                             f" at address: '{node.address}'"
    }
    warning_messages = {
        'CRAWL_SKIPPED': f"service '{node.service_name}' discovered but crawling skipped by configuration",
        'CYCLE': f"service '{node.service_name}' discovered as a parent of itself!",
        'DEFUNCT': f"service '{node.service_name}' configuration present on parent, but it not in use!"
    }

    # warnings verbose display
    if node.warnings:
        for warning in node.warnings:
            print(error_prefix + colored_force(f"└> WARN: ({warning}): ", "yellow") + warning_messages[warning],
                  file=out)

    # error verbose display
    if node.errors:
        for error in node.errors:
            print(error_prefix + colored_force(f"└> ERROR: ({error}): ", "red") + error_messages[error],
                  file=out)


async def _wait_for_service_names(nodes: Dict[str, Node], depth: int) -> None:
    """
    Wait for 'service_name' to be discovered in all nodes

    :param nodes:
    :param depth:
    :return: None
    """
    for _ in range(100):
        for node in nodes.values():
            if not node.name_lookup_complete():
                await asyncio.sleep(1)
                break
        else:
            break

        if constants.ARGS.debug:
            remaining = _remaining_nodes_for_debugging(nodes)
            logs.logger.debug(f"Waiting for remaining {str(len(remaining))} service names before rendering ascii "
                              f"at depth {str(depth)}...")
            logs.logger.debug(remaining)
            await asyncio.sleep(5)
    else:
        remaining = _remaining_nodes_for_debugging(nodes)
        print(colored_force(f"Infinite wait interrupted waiting for {len(remaining)} services names for: ", "red"))
        print(colored_force(constants.PP.pformat(remaining), "yellow"))
        sys.exit(1)


def _remaining_nodes_for_debugging(nodes: Dict[str, Node]) -> dict:
    return {node_ref: {**asdict(node), 'crawl_strategy': node.crawl_strategy.name}
            for node_ref, node in nodes.items() if not node.name_lookup_complete()}


def _merge_nodes_by_service_name(nodes: Dict[str, Node]) -> Dict[str, Node]:
    """
    Return a dict of nodes, merging input nodes with the same protocol and 'service_name'
    into one node.  Merges the associated protocol_mux's

    :param nodes: the unmerged dictionary of Node()s
    :return: the merged dictionary of Node()s
    """
    merged_nodes = {}
    for node_ref, node in nodes.items():
        synthetic_node_ref = _synthesize_node_ref(node, node_ref)
        if synthetic_node_ref not in merged_nodes:
            merged_nodes[synthetic_node_ref] = node
        elif node.protocol_mux not in merged_nodes[synthetic_node_ref].protocol_mux:
            merged_nodes[synthetic_node_ref].protocol_mux = ','.join(
                [merged_nodes[synthetic_node_ref].protocol_mux, node.protocol_mux]
            )

    return merged_nodes


def _synthesize_node_ref(node: Node, default: str) -> str:
    """
    Synthesize a node_ref using node attributes

    :param node: - the Node() object
    :param default: - default to use if node_ref not synthesizable
    :return: a 'synthetic' node_ref
    """
    if node.service_name:
        return f"{node.protocol.ref.lower()}_{node.service_name}"

    return default
