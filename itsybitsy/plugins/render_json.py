# Copyright # Copyright 2020 Life360, Inc
# SPDX-License-Identifier: Apache-2.0

import json
from dataclasses import asdict, is_dataclass
from typing import Dict

from itsybitsy import constants, renderers
from itsybitsy.charlotte import CrawlStrategy
from itsybitsy.charlotte_web import Protocol
from itsybitsy.node import Node


class RendererJson(renderers.RendererInterface):
    @staticmethod
    def ref() -> str:
        return 'json'

    def render(self, tree: Dict[str, Node]):
        tree_with_args = _add_cli_args_to_json_tree(tree)
        print(json.dumps(tree_with_args, cls=_EnhancedJSONEncoder))


class _EnhancedJSONEncoder(json.JSONEncoder):
    """Dataclass objects to not have native support for JSON serialization. This class allows for that"""
    def default(self, o):
        if is_dataclass(o):
            return asdict(o)
        return super().default(o)


def _deserialize_object(dct: dict):
    """Used to json deserialization to our custom classes"""
    dct_type = dct.get('__type__')
    if not dct_type:
        return dct

    if 'Node' == dct_type:
        return Node(**dct)
    elif 'CrawlStrategy' == dct_type:
        return CrawlStrategy(**dct)
    elif 'Protocol' == dct_type:
        return Protocol(**dct)

    e_str = f"Unrecognized __type__: ({dct_type}) encountered during json deserialization"
    raise Exception(e_str)  # pylint: disable=broad-exception-raised


def load(file):
    """
    load json rendering of tree from `file`, parse requisite outputs

    :param file:
    :return:
    """
    with open(file, encoding="utf8") as f:
        loaded = json.load(f, object_hook=_deserialize_object)
        constants.ARGS.max_depth = int(loaded['args']['max_depth'])
        constants.ARGS.skip_nonblocking_grandchildren = loaded['args']['skip_nonblocking_grandchildren']

        return loaded['tree']


def dump(tree: Dict[str, Node], file: str = None) -> None:
    """
    dump json of the tree to a file - includes globals.ARGS in the dump
    :param tree:
    :param file:
    :return:
    """
    tree_with_args = _add_cli_args_to_json_tree(tree)
    with open(file, 'w+', encoding="utf8") as file_handle:
        json.dump(tree_with_args, file_handle, cls=_EnhancedJSONEncoder)


def _add_cli_args_to_json_tree(tree: Dict[str, Node]) -> dict:
    return {
        'args': vars(constants.ARGS),
        'tree': tree
    }
