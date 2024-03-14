import argparse
from typing import List, Optional

import configargparse


command_spider = 'spider'
command_render = 'render'
argparser: Optional[configargparse.ArgParser] = None
spider_subparser: Optional[configargparse.ArgParser] = None
render_subparser: Optional[configargparse.ArgParser] = None


def parse_args(registered_renderer_refs: List[str]) -> (configargparse.Namespace, list):
    class ConciseHelpFormatter(argparse.ArgumentDefaultsHelpFormatter):
        """Custom formatter to reduce redundancy in Help Display Output"""
        def _format_action_invocation(self, action):
            if not action.option_strings or action.nargs == 0:
                return super()._format_action_invocation(action)
            # we only care about `-s ARGS, --long ARGS`
            default = self._get_default_metavar_for_optional(action)
            args_string = self._format_args(action, default)
            return ', '.join(action.option_strings) + ' ' + args_string

    global argparser, spider_subparser, render_subparser
    argparser = configargparse.ArgumentParser(
        description=(
            "Give it (a) seed host(s).\n"
            "It will crawl them.\n"
            "And climb.\n"
            "The waterspout.\n\n"
        )
    )

    def formatter_class(prog):
        return ConciseHelpFormatter(prog, max_help_position=100, width=200)
    # subparsers
    subparsers = argparser.add_subparsers(
        help=f'Please select an acceptable command for itsybitsy: "{command_spider}" or "{command_render}"'
    )
    subparsers.required = True
    subparsers.dest = 'command'
    spider_p = subparsers.add_parser(command_spider, help='Crawl a network of services - given a seed',
                                     formatter_class=formatter_class, default_config_files=['./spider.conf'])
    render_p = subparsers.add_parser(command_render, help='Render results of a previous crawl',
                                     formatter_class=formatter_class)
    spider_subparser = spider_p
    render_subparser = render_p

    # add common opts to each sub parser
    for sub_p in subparsers.choices.values():
        # common args
        sub_p.add_argument('-D', '--hide-defunct', action='store_true', help='Hide defunct (unused) connections')
        sub_p.add_argument('-o', '--output', action='append', choices=registered_renderer_refs,
                           help='Format in which to output the final graph.  Available options: '
                                f"[{','.join(registered_renderer_refs)}]")
        sub_p.add_argument('--debug', action='store_true', help='Log debug output to stderr')

    # spider command args
    spider_p.add_argument('-s', '--seeds', required=True, nargs='+', metavar='SEED',
                          help='Seed host(s) to begin crawling viz. an IP address or hostname.  Must be in the format: '
                               '"provider:address".  e.g. "ssh:10.0.0.42" or "k8s:widget-machine-5b5bc8f67f-2qmkp')
    spider_p.add_argument('-t', '--timeout', type=int, default=60, metavar='TIMEOUT',
                          help='Timeout when crawling a node')
    spider_p.add_argument('-d', '--max-depth', type=int, default=100, metavar='DEPTH', help='Max tree depth to crawl')
    spider_p.add_argument('-c', '--config-file', is_config_file=True, metavar='FILE', help='Specify a config file path')
    spider_p.add_argument('-X', '--disable-providers', nargs='+', default=[], metavar='PROVIDER',
                          help="Do not initialize or crawl with these providers")
    spider_p.add_argument('-P', '--skip-protocols', nargs='+', default=[], metavar='PROTOCOL',
                          help='A list of protocols to skip.  e.g. "NSQ PXY"')
    spider_p.add_argument('-M', '--skip-protocol-muxes', nargs='+', default=[], metavar='MUX',
                          help='Skip crawling for children on services with these '
                               'names (name lookup will still happen)')
    spider_p.add_argument('-G', '--skip-nonblocking-grandchildren', action='store_true',
                          help='Skip crawling of nonblocking children unless they '
                               'are direct children of the seed nodes')
    spider_p.add_argument('-x', '--obfuscate', action='store_true',
                          help="Obfuscate graph details.  Useful for sharing rendered output outside of "
                               "trusted organizations.")
    spider_p.add_argument('-q', '--quiet', action='store_true',
                          help='Do not render graph output to stdout while crawling')

    # render command args
    render_p.add_argument('-f', '--json-file', metavar='FILE',
                          help='Instead of crawling, load and render a json serialization of the tree')

    return argparser.parse_known_args()