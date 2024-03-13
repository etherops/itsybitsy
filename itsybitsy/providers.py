# Copyright # Copyright 2020 Life360, Inc
# SPDX-License-Identifier: Apache-2.0

import configargparse
from typing import List, Optional

from . import constants, logs
from .charlotte_web import Hint
from .node import NodeTransport
from .plugin_core import PluginInterface, PluginFamilyRegistry


class TimeoutException(Exception):
    """Timeout occurred connecting to the provider"""


class CreateNodeTransportException(Exception):
    """An exception during creation of Node Transport"""


class ProviderInterface(PluginInterface):
    @staticmethod
    def is_container_platform() -> bool:
        """
        Optionally announce whether this provider is a container based platform (kubernetes, docker).  This is used to
        render container nodes differently than traditional servers systems.
        :return:
        """
        return False

    async def open_connection(self, address: str) -> Optional[type]:
        """
        Optionally open a connection which can then be passed into lookup_name() and crawl()

        :param address: for example, and ip address for which to open and ssh connection
        :return: mixed type object representing a connection to node in the provider
        :raises:
            TimeoutException - Timeout connecting to provider for name lookup
        """
        del address
        return None

    async def lookup_name(self, address: str, connection: Optional[type]) -> Optional[str]:
        """
        Takes and address and lookups up service name in provider.  Default response when subclassing
        will be a no-op, which allows provider subclasses to only implement aspects of this classes functionality
        a-la-cart style

        :param address: look up the name for this IP address
        :param connection: optional connection.  for example if an ssh connection was opened during
                                   lookup_name() it can be returned there and re-used here
        :return: the derived service name in string form
        :raises:
            NameLookupFailedException - Not able to find a name in the provider
        """
        del address, connection
        return None

    async def take_a_hint(self, hint: Hint) -> List[NodeTransport]:
        """
        Takes a hint, looks up an instance of service in the provider, and returns a NodeTransport representing the
        Node discovered in the Provider. Default response when subclassing will be a no-op, which allows provider
        subclasses to only implement aspects of this classes functionality a-la-cart style.
        Please return the NodeTransport object in the form of a List of 1 NodeTransport object!
        :param hint: take this hint
        :return:
        """
        del hint
        return []

    async def crawl_downstream(self, address: str, connection: Optional[type], **kwargs) -> List[NodeTransport]:
        """
        Crawl provider for downstream services using CrawlStrategy.  Default response when subclassing will be a no-op,
        which allows provider subclasses to only implement aspects of this classes functionality a-la-cart style.
        Please cache your results to improve system performance!

        :param address: address to crawl
        :param connection: optional connection.  for example if an ssh connection was opened during
                                   lookup_name() it can be returned there and re-used here
        :Keyword Arguments: extra arguments passed to provider from CrawlStrategy.provider_args

        :return: the children as a list of Node()s
        """
        del address, kwargs, connection
        return []


_provider_registry = PluginFamilyRegistry(ProviderInterface)


def parse_provider_args(argparser: configargparse.ArgParser):
    _provider_registry.parse_plugin_args(argparser, constants.ARGS.disable_providers)


def register_providers():
    _provider_registry.register_plugins(constants.ARGS.disable_providers)


def get_provider_by_ref(provider_ref: str) -> ProviderInterface:
    return _provider_registry.get_plugin(provider_ref)


def parse_crawl_strategy_response(response: str, address: str, command: str) -> List[NodeTransport]:
    lines = response.splitlines()
    if len(lines) < 2:
        return []
    header_line = lines.pop(0)
    node_transports = [_create_node_transport_from_crawl_strategy_response_line(header_line, data_line)
                       for data_line in lines]
    logs.logger.debug(f"Found {len(node_transports)} children for {address}, command: \"{command[:100]}\"..")
    return node_transports


def _create_node_transport_from_crawl_strategy_response_line(header_line: str, data_line: str):
    field_map = {
        'mux': 'protocol_mux',
        'address': 'address',
        'id': 'debug_identifier',
        'conns': 'num_connections',
        'metadata': 'metadata'
    }
    fields = {}
    for label, value in zip(header_line.split(), data_line.split()):
        if label == 'address' and value == 'null':
            continue
        fields[label] = value

    # field transforms/requirements
    if 'mux' not in fields:
        raise CreateNodeTransportException(f"protocol_mux missing from crawl strategy results")
    if 'metadata' in fields:
        fields['metadata'] = dict(tuple(i.split('=') for i in fields['metadata'].split(',')))
    if 'conns' in fields:
        fields['conns'] = int(fields['conns'])
    return NodeTransport(**{field_map[k]: v for k, v in fields.items() if v})
