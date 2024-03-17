# Copyright # Copyright 2020 Life360, Inc
# SPDX-License-Identifier: Apache-2.0

from termcolor import colored
from typing import Dict, List, Optional
import configargparse
import importlib
import pkgutil
import re
import sys

import itsybitsy.plugins


def import_plugin_classes():
    for _1, name, _2 in pkgutil.iter_modules(itsybitsy.plugins.__path__,
                                             itsybitsy.plugins.__name__ + "."):
        importlib.import_module(name)


class PluginArgParser:
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
        option_name = f"{self._prefix}-{option_name}"
        option_name_with_dashes_consoliated = re.sub('-+', '-', option_name)
        option_name_with_leading_dashes = f"--{option_name_with_dashes_consoliated}"
        self._argparser.add_argument(option_name_with_leading_dashes, **kwargs)


class PluginRefNotImplemented(Exception):
    """Exception thrown if provider has not implemented ref() method"""


class PluginClobberException(Exception):
    """An exception indicating a provider is clobbering the namespace of another plugin"""


class PluginInterface:
    @staticmethod
    def ref() -> str:
        """
        Every plugin is identified by a unique "reference" or "ref" which much be declared by implemented this
        public abstract method.
        :return: the unique reference or "ref" of the provider.
        """
        raise PluginRefNotImplemented

    @staticmethod
    def register_cli_args(argparser: PluginArgParser):
        """Each plugin has a chance to register custom CLI args which will be prefixed with `self.ref()` """

    def __str__(self):
        return self.ref()


class PluginFamilyRegistry:
    """Registry for plugins within a plugin Family"""
    def __init__(self, cls: PluginInterface, cli_args_prefix: str = ''):
        self._cls: PluginInterface = cls
        self._cli_args_prefix = cli_args_prefix
        self._plugin_registry: Dict[str, PluginInterface] = {}

    def parse_plugin_args(self, argparser: configargparse.ArgParser, disabled_classes: Optional[List[str]] = None):
        """Plugins are given an opportunity to register custom CLI arguments"""
        for plugin in [c for c in self._cls.__subclasses__() if c.ref() not in (disabled_classes or [])]:
            plugin: PluginInterface
            prefix = f'{self._cli_args_prefix}-{plugin.ref()}' if self._cli_args_prefix else plugin.ref()
            plugin_argparser = PluginArgParser(prefix, argparser)
            plugin.register_cli_args(plugin_argparser)

    def register_plugins(self, disabled_classes: Optional[List[str]] = None):
        for plugin in [c for c in self._cls.__subclasses__() if c.ref() not in (disabled_classes or [])]:
            if plugin.ref() in self._plugin_registry:
                raise PluginClobberException(f"Provider {plugin.ref()} already registered!")
            self._plugin_registry[plugin.ref()] = plugin()

    def get_plugin(self, ref: str) -> PluginInterface:
        try:
            return self._plugin_registry[ref]
        except KeyError as e:
            print(colored(f"Attempted to load invalid plugin: {ref}", 'red'))
            print(colored(e, 'yellow'))
            sys.exit(1)

    def get_registered_plugin_refs(self) -> List[str]:
        return list(self._plugin_registry.keys())
