import configargparse
import re
from typing import Dict, List

from .node import Node
from .plugin_core import PluginInterface, PluginFamilyRegistry


class RendererArgParser:
    def __init__(self, prefix: str, argparser: configargparse.ArgParser):
        self._prefix = prefix
        self._argparser = argparser

    def add_argument(self, option_name: str, **kwargs):
        """
        A wrapper method on top of the classic ArgParse::add_argument().  All keyword arguments are supported, however
        only a single option_name is allowed, such as '--foo-argument'.  Argument registered here will be prepended
        with the ProviderInterface() ref in order to avoid namespace collisions between provider plugins.  For example
        '--foo-argument' registered by a ProviderInterface() with ref() = 'bar' will result in a CLI arg of
        '--bar-foo-argument'.

        :param option_name: such as '--foo-something'
        :param kwargs: pass through kwargs for ArgParse::add_argument, such as "required", "type", "nargs", etc.
        :return:
        """
        option_name = f"render-{self._prefix}-{option_name}"
        option_name_with_dashes_consoliated = re.sub('-+', '-', option_name)
        option_name_with_leading_dashes = f"--{option_name_with_dashes_consoliated}"
        self._argparser.add_argument(option_name_with_leading_dashes, **kwargs)


class RenderNotImplemented(Exception):
    """Exception thrown if provider has not implemented render() method"""


class RendererInterface(PluginInterface):
    def render(self, tree: Dict[str, Node]):
        """Please render the tree of nodes when asked to do so!"""
        raise RenderNotImplemented


_renderer_registry = PluginFamilyRegistry(RendererInterface, 'render')


def parse_renderer_args(argparser: configargparse.ArgParser):
    _renderer_registry.parse_plugin_args(argparser)


def register_renderers():
    _renderer_registry.register_plugins([])


def get_renderer_by_ref(renderer_ref: str) -> RendererInterface:
    return _renderer_registry.get_plugin(renderer_ref)


def get_renderer_refs() -> List[str]:
    return [cls.ref() for cls in RendererInterface.__subclasses__()]


def merge_hints(nodes: Dict[str, Node]) -> Dict[str, Node]:
    """
    Merge a regular nodes and hint nodes by protocol and protocol mux (multiplexer).  If there are 2 nodes in the input
    that have share the same protocol and protocol_mux, and one is from a Hint - merge them together so that they are
    displayed as one edge.

    :param nodes:
    :return:
    """
    hints = {_protocol_and_mux(node): (node_ref, node)
             for node_ref, node in nodes.items() if node.from_hint}
    if 0 == len(hints):
        return nodes

    not_hints = {node_ref: node for node_ref, node in nodes.items() if not node.from_hint}
    used_hints = []
    merged_nodes = {ref: node for ref, node in not_hints.items() if _protocol_and_mux(node) not in hints}
    mergeable_nodes = {ref: node for ref, node in not_hints.items() if _protocol_and_mux(node) in hints}
    for node_ref, node in mergeable_nodes.items():
        protocol_and_mux = _protocol_and_mux(node)
        merged_node = _merge_node_and_hint(node, hints[protocol_and_mux][1])
        merged_nodes[node_ref] = merged_node
        used_hints.append(protocol_and_mux)
    unused_hints = {ref: node for ref, node in nodes.items() if f"{node.protocol.ref}.{node.protocol_mux}"
                    not in used_hints}
    merged_nodes.update(unused_hints)

    return merged_nodes


def _merge_node_and_hint(node: Node, hint: Node) -> Node:
    node.from_hint = True
    node.address = node.address or hint.address
    node.containerized = node.containerized or hint.containerized
    node.service_name = node.service_name or hint.service_name
    node.warnings = {**node.warnings, **hint.warnings}
    node.errors = {**node.errors, **hint.errors}
    node_children = node.children or {}
    hint_children = hint.children or {}
    node.children = {**hint_children, **node_children}

    return node


def _protocol_and_mux(node: Node) -> str:
    return f"{node.protocol.ref}.{node.protocol_mux}"


def clean_service_name(name: str) -> str:
    return name.replace('"', '').replace(':', '_').replace('#', '_')