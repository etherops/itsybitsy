# Copyright # Copyright 2020 Life360, Inc
# SPDX-License-Identifier: Apache-2.0

"""
Assumptions:
    - A Jump/Bastion server is used
    - The Jump/Bastion server is configured in an ssh config file per host, default ~/.ssh/config
"""
import asyncio
import asyncssh
import os
import paramiko
import getpass
import sys

from asyncssh import ChannelOpenError, SSHClientConnection
from termcolor import colored
from typing import List, Optional

from itsybitsy import constants, logs
from itsybitsy.providers import ProviderInterface, TimeoutException, parse_crawl_strategy_response
from itsybitsy.plugin_core import PluginArgParser
from itsybitsy.node import NodeTransport

bastion: Optional[SSHClientConnection] = None
connect_timeout = 5
connection_semaphore = None
connection_semaphore_spaces_used = 0
connection_semaphore_spaces_min = 10
ssh_connect_args = {'known_hosts': None}


class ProviderSSH(ProviderInterface):
    @staticmethod
    def ref() -> str:
        return 'ssh'

    @staticmethod
    def register_cli_args(argparser: PluginArgParser):
        argparser.add_argument('--bastion-timeout', type=int, default=10, metavar='TIMEOUT',
                               help='Timeout in seconds to establish SSH connection to bastion (jump server)')
        argparser.add_argument('--concurrency', type=int, default=10, metavar='CONCURRENCY',
                               help='Max number of concurrent SSH connections')
        argparser.add_argument('--config-file', default="~/.ssh/config", metavar='FILE',
                               help='SSH config file to parse for configuring SSH sessions.  '
                                    'As in `ssh -F ~/.ssh/config`)')
        argparser.add_argument('--passphrase', action='store_true',
                               help='Prompt for, and use the specified passphrase to decrype SSH private keys')
        argparser.add_argument('--name-command', required=True, metavar='COMMAND',
                               help='Used by SSH Provider to determine node name')

    async def open_connection(self, address: str) -> SSHClientConnection:
        if not bastion:
            await _configure(address)
        logs.logger.debug(f"Getting asyncio SSH connection for host {address}")
        async with connection_semaphore:
            return await _get_connection(address)

    async def lookup_name(self, address: str, connection: SSHClientConnection) -> str:
        logs.logger.debug(f"Getting service name for address {address}")
        node_name_command = constants.ARGS.ssh_name_command
        async with connection_semaphore:
            result = await connection.run(node_name_command, check=True)
        node_name = result.stdout.strip()
        logs.logger.debug(f"Discovered name: {node_name} for address {address}")

        return node_name

    async def crawl_downstream(self, address: str, connection: SSHClientConnection, **kwargs) -> List[NodeTransport]:
        try:
            command = kwargs['shell_command']
        except IndexError as e:
            print(colored(f"Crawl Strategy incorrectly configured for provider SSH.  "
                          f"Expected **kwargs['shell_command']. Got:{str(kwargs)}", 'red'))
            raise e
        response = await connection.run(command)
        if response.stdout.strip().startswith('ERROR:'):
            raise Exception('CRAWL ERROR: ' + response.stdout.strip().replace("\n", "\t"))
        return parse_crawl_strategy_response(response.stdout.strip(), address, command)


async def _get_connection(host: str, retry_num=0) -> asyncssh.SSHClientConnection:
    try:
        logs.logger.debug(f"Getting asyncio SSH connection for host {host}")
        return await bastion.connect_ssh(host, **ssh_connect_args)
    except ChannelOpenError:
        raise TimeoutException(f"asyncssh.ChannelOpenError encountered opening SSH connection for {host}")
    except Exception as e:
        if retry_num < 3:
            asyncio.ensure_future(_occupy_one_sempahore_space())
            await asyncio.sleep(.1)
            return await _get_connection(host, retry_num+1)
        raise e


async def _occupy_one_sempahore_space() -> None:
    """Use up one spot in the SSH connection semaphore.

       This is used to fine tune whether the semaphore is configured
       for too many concurrent SHH connection.  It will not occupy more
       than that which leaves {semaphore_spaces_min} spaces in the
       semaphore for real work.
    """
    global connection_semaphore_spaces_used

    if (constants.ARGS.ssh_concurrency - connection_semaphore_spaces_used) > connection_semaphore_spaces_min:
        async with connection_semaphore:
            connection_semaphore_spaces_used += 1
            logs.logger.debug(f"Using 1 additional semaphore space, ({connection_semaphore_spaces_used} used)")
            forever_in_the_context_of_this_program = 86400
            await asyncio.sleep(forever_in_the_context_of_this_program)


# configuration private functions
async def _configure(address: str):
    global bastion, connection_semaphore, ssh_connect_args
    connection_semaphore = asyncio.BoundedSemaphore(constants.ARGS.ssh_concurrency)
    ssh_config = _get_ssh_config_for_host(address)
    jump_server_address = _get_jump_server_for_host(ssh_config)
    ssh_connect_args['username'] = ssh_config.get('user')
    if constants.ARGS.ssh_passphrase:
        ssh_connect_args['passphrase'] = getpass.getpass(colored("Enter SSH key passphrase:", 'green'))

    try:
        bastion = await asyncio.wait_for(
            asyncssh.connect(jump_server_address, **ssh_connect_args), timeout=constants.ARGS.ssh_bastion_timeout
        )
    except asyncio.TimeoutError:
        print(colored(f"Timeout connecting to SSH bastion server: {jump_server_address}.  "
                      f"Try turning it off and on again.", 'red'))
        sys.exit(1)
    except asyncssh.PermissionDenied:
        print(colored(f"SSH Permission denied attempting to connect to {address}.  It is possible that your SSH Key "
                      f"requires a passphrase.  If this is the case please add either it to ssh-agent with `ssh-add` "
                      f"(See https://www.ssh.com/ssh/add for details on that process) or try again using the "
                      f"--ssh-passphrase argument.  ", 'red'))
        sys.exit(1)


def _get_ssh_config_for_host(host: str) -> dict:
    """Parse ssh config file to retrieve bastion address and username

    :param host: (str) host to parse ssh config file for
    :return: a dict of ssh config, e.g.
        {
            'forwardagent': 'yes',
            'hostname': '10.0.0.145',
            'proxycommand': 'ssh -q ops nc 10.0.0.145 22',
            'serveraliveinterval': '120',
            'stricthostkeychecking': 'no',
            'user': 'foo',
            'userknownhostsfile': '/dev/null'
        }
    """
    ssh_config = paramiko.SSHConfig()
    user_config_file = os.path.expanduser(constants.ARGS.ssh_config_file)
    try:
        with open(user_config_file) as f:
            ssh_config.parse(f)
    except FileNotFoundError:
        print("{} file could not be found. Aborting.".format(user_config_file))
        sys.exit(1)

    return ssh_config.lookup(host)


def _get_jump_server_for_host(config: dict) -> str:
    """
    :param config: ssh config in dict format as returned by paramiko.SSHConfig().lookup()
    """
    config_file_path = os.path.expanduser(constants.ARGS.ssh_config_file)
    proxycommand_host = _get_proxycommand_host(config)
    proxyjump_host = _get_proxyjump_host(config)
    bastion_host = proxyjump_host or proxycommand_host

    if not bastion_host:
        print(colored(f"Required SSH directive ProxyJump (or ProxyCommand) not found (or misconfigured)"
                      f" in {config_file_path}... Please correct your ssh config! SSH directives found:", 'red'))
        constants.PP.pprint(config)
        sys.exit(1)

    bastion_config = _get_ssh_config_for_host(bastion_host)

    if 'hostname' not in bastion_config:
        print(colored(f"{bastion_host} misconfigured in {config_file_path}...  "
              f"Please correct your ssh config! Contents:", 'red'))
        constants.PP.pprint(config)
        sys.exit(1)

    return bastion_config['hostname']


def _get_proxycommand_host(config):
    if 'proxycommand' not in config:
        return None

    proxycommand_columns = config['proxycommand'].split(" ")

    if 6 != len(proxycommand_columns):
        return None

    return proxycommand_columns[2]


def _get_proxyjump_host(config):
    if 'proxyjump' not in config:
        return None

    return config['proxyjump']
