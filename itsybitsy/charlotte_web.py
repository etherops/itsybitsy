# Copyright # Copyright 2020 Life360, Inc
# SPDX-License-Identifier: Apache-2.0

"""
The charlotte web module contains hint about edges in the graph/web provided by users in web.yaml.  web.yaml also
allows for definition of edges which are to be skipped.  (It has nothing to do  with the "world wide web")
"""
import os
import sys
from dataclasses import dataclass
from typing import NamedTuple, Dict, List
from termcolor import colored
from yaml import safe_load

from . import constants


# using this instead of a namedtuple for ease of json serialization/deserialization
@dataclass(frozen=True)
class Protocol:
    ref: str
    name: str
    blocking: bool
    is_database: bool = False
    __type__: str = 'Protocol'  # for json serialization/deserialization


class Hint(NamedTuple):
    service_name: str
    protocol: Protocol
    protocol_mux: str
    provider: str
    instance_provider: str


class WebYamlException(Exception):
    """Errors parsing web.yaml file"""


_web_file = 'web.yaml'
_hints:  Dict[str, List[Hint]] = {}
_protocols: Dict[str, Protocol] = {}
_skip_service_names: List[str] = []
_skip_protocol_muxes: List[str] = []

PROTOCOL_SEED = Protocol('SEED', 'Seed', True)
PROTOCOL_HINT = Protocol('HNT', 'Hint', True)
_builtin_protocols = {PROTOCOL_SEED, PROTOCOL_HINT}
_protocols['SEED'] = PROTOCOL_SEED
_protocols['HNT'] = PROTOCOL_HINT


def spin_up():
    """It initializes the web from web.yaml"""
    with open(os.path.join(constants.CHARLOTTE_DIR, _web_file), 'r', encoding='utf-8') as stream:
        configs = _parse_yaml_config(stream)
        _parse_protocols(configs)
        _parse_skips(configs)

        # hints
        global _hints  # pylint: disable=global-variable-not-assigned
        if configs.get('hints'):
            for service_name, lst in configs.get('hints').items():
                try:
                    _hints[service_name] = [Hint(**dict(dct, **{'protocol': get_protocol(dct['protocol'])})) for dct in lst]
                except TypeError:
                    print(colored(f"Hints malformed in {_web_file}.  Fields expected: {Hint._fields}", 'red'))
                    print(colored(lst, 'yellow'))
                    sys.exit(1)

        # validate
        _validate()


def _parse_yaml_config(stream) -> Dict[str, dict]:
    try:
        return safe_load(stream)
    except Exception as e:
        raise WebYamlException(f"Unable to load yaml {_web_file}") from e


def _parse_protocols(configs: Dict[str, dict]) -> None:
    if not configs.get('protocols'):
        return

    try:
        for protocol, attrs in configs.get('protocols').items():
            _protocols[protocol] = Protocol(ref=protocol, **attrs)
    except Exception as e:
        raise WebYamlException(f"protocols malformed in {_web_file}") from e


def _parse_skips(configs: Dict[str, dict]) -> None:
    global _skip_service_names, _skip_protocol_muxes
    _skip_service_names = configs.get('skips').get('service_names') if configs.get('skips') else []
    _skip_protocol_muxes = configs.get('skips').get('protocol_muxes') if configs.get('skips') else []


def skip_service_name(service_name: str) -> bool:
    return True in [match in service_name for match in _skip_service_names]


def skip_protocol_mux(protocol_mux: str) -> bool:
    return True in [match in protocol_mux for match in _skip_protocol_muxes]


def hints(service_name: str) -> List[Hint]:
    return _hints.get(service_name) or []


def get_protocol(ref: str) -> Protocol:
    try:
        return _protocols[ref]
    except KeyError as e:
        print(colored(f"Protocol {ref} not found!  Please validate your configurations in {constants.CHARLOTTE_DIR}",
                      'red'))
        raise e


def _validate() -> None:
    if len(_protocols) <= len(_builtin_protocols):
        print(
            colored('No protocols defined in charlotte.d/web.yaml!  Please define protocols before proceeding', 'red')
        )
        sys.exit(1)
