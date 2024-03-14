# Copyright # Copyright 2020 Life360, Inc
# SPDX-License-Identifier: Apache-2.0

import typing
import os
import re
from dataclasses import dataclass, asdict
from string import Template
from typing import List, Optional
from yaml import safe_load_all

from . import charlotte_web, constants, logs


class CrawlStrategyException(Exception):
    """Exceptions for CrawlStrategy"""


@dataclass(frozen=True)
class CrawlStrategy:
    description: str
    name: str
    protocol: charlotte_web.Protocol
    providers: List[str]
    provider_args: dict
    child_provider: dict
    service_name_filter: dict
    service_name_rewrites: dict
    __type__: str = 'CrawlStrategy'  # for json serialization/deserialization

    def filter_service_name(self, service_name: str) -> bool:
        """
        Crawl strategy may filter out this service-name. True response means this service_name should not be crawled.

        :param service_name:
        :return:
        """
        if not self.service_name_filter:
            return False

        not_filters = self.service_name_filter.get('not')
        only_filters = self.service_name_filter.get('only')
        if not_filters and service_name in not_filters:
            return True
        if only_filters and service_name not in only_filters:
            return True

        return False

    def determine_child_provider(self, protocol_mux: str, address: str = None) -> Optional[str]:
        """
        Determine the provider for the protocol_mux of a node discovered using this crawl strategy

        :param protocol_mux: the protocol mux (port, nsq channel...)
        :param address: address of the node
        :return: a string representation of the provider
        """
        if 'matchAll' == self.child_provider['type']:
            return self.child_provider['provider']

        if 'matchAddress' == self.child_provider['type']:
            for match, provider in self.child_provider['matches'].items():
                if re.search(match, address or ''):
                    return provider
            return self.child_provider['default']

        if 'matchPort' == self.child_provider['type']:
            try:
                if int(protocol_mux) in self.child_provider['matches']:
                    return self.child_provider['matches'][int(protocol_mux)]
                return self.child_provider['default']
            except (ValueError, IndexError):
                return self.child_provider['default']

        logs.logger.fatal("child provider match type: %s not supported", self.child_provider['type'])
        raise CrawlStrategyException()

    def rewrite_service_name(self, service_name: str, node) -> str:
        """
        Some service names have to be filtered/rewritten.  It uses string.Template
        to re-write the service name based on node attributes.

        :param service_name: the service name to rewrite, if needed
        :param node: used to interpolate attributes into the rewrite
        :return:
        """
        for match, rewrite in self.service_name_rewrites.items():
            if service_name and match in service_name:
                return Template(rewrite).substitute(dict(asdict(node)))

        return service_name


crawl_strategies: typing.List[CrawlStrategy] = []
_seed_crawl_strategy_child_provider = {'type': 'matchAll', 'provider': constants.PROVIDER_SSH}
SEED_CRAWL_STRATEGY = CrawlStrategy('Seed Crawl Strategy', 'Seed', charlotte_web.PROTOCOL_SEED,
                                    [constants.PROVIDER_SEED], '', _seed_crawl_strategy_child_provider, {}, {})
HINT_CRAWL_STRATEGY = CrawlStrategy('Hint Crawl Strategy', 'Hint', charlotte_web.PROTOCOL_HINT,
                                    [constants.PROVIDER_HINT], '', {}, {}, {})


def init():
    charlotte_web.spin_up()
    _load_crawl_strategies()


def _load_crawl_strategies():
    for file in os.listdir(constants.CHARLOTTE_DIR):
        if file.endswith('.yaml'):
            with open(os.path.join(constants.CHARLOTTE_DIR, file), 'r', encoding='utf-8') as stream:
                dcts = safe_load_all(stream)
                for dct in dcts:
                    if 'CrawlStrategy' == dct.get('type'):
                        protocol = charlotte_web.get_protocol(dct['protocol'])
                        cs = CrawlStrategy(
                            dct['description'],
                            dct['name'],
                            protocol,
                            dct['providers'],
                            dct['providerArgs'],
                            dct['childProvider'],
                            dct['serviceNameFilter'] if 'serviceNameFilter' in dct else {},
                            dct['serviceNameRewrites'] if 'serviceNameRewrites' in dct else {}
                        )
                        crawl_strategies.append(cs)
                        logs.logger.debug('Loaded CrawlStrategy:')
                        logs.logger.debug(cs)
