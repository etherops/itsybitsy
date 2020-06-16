"""
Assumptions
     - All services exist in 1 and only 1 AWS account
     - An authenticated AWS sessions exists in execution context using the default AWS authentication chain, or
        using the user specific --aws-profile argument
     - Instances of a service can be looked up in AWS by querying for user specified tag
"""
import boto3
import re
import sys
from botocore.exceptions import ClientError
from termcolor import colored
from typing import List, Optional

from .. import constants, logs
from ..node import NodeTransport
from ..charlotte_web import Hint
from ..providers import ProviderArgParser, ProviderInterface

tag_name_pos = 0
tag_value_pos = 1


class ProviderAWS(ProviderInterface):
    def __init__(self):
        if constants.ARGS.aws_profile:
            boto3.setup_default_session(profile_name=constants.ARGS.aws_profile)
        self.ec2_client = boto3.client('ec2')
        self.tag_filters = {tag_filter.split('=')[tag_name_pos]: tag_filter.split('=')[tag_value_pos]
                            for tag_filter in constants.ARGS.aws_tag_filters}

    @staticmethod
    def ref() -> str:
        return 'aws'

    @staticmethod
    def register_cli_args(argparser: ProviderArgParser):
        argparser.add_argument('--profile',  help='AWS Credentials file profile to use.  '
                                                  'This will override the AWS_PROFILE environment variable.')
        argparser.add_argument('--service-name-tag', required=True, metavar='TAG',
                               help='AWS tag associated with service name')
        argparser.add_argument('--tag-filters', nargs='*',  metavar='FILTER',
                               help='Additional AWS tags to filter on or services.  Specified in format: '
                                    '"TAG_NAME=VALUE" pairs')

    async def lookup_name(self, address: str, _: None) -> Optional[str]:
        logs.logger.debug(f"Performing AWS name lookup for {address}")
        try:
            response = self.ec2_client.describe_network_interfaces(
                Filters=[{
                    'Name': 'addresses.private-ip-address',
                    'Values': [address]
                }]
            )
        except ClientError as e:
            _die(e)

        # parse name from response
        name = None
        try:
            description = response['NetworkInterfaces'][0]['Description']
            if description.startswith('ElastiCache'):
                name = description.replace(' ', '-').lower()
                name = re.sub(r'[0-9\-]{2,}', '', name)
            elif description.startswith('RDSNetworkInterface'):
                name = f"{description}_{response['NetworkInterfaces'][0]['RequesterId']}"
        except (KeyError, IndexError):
            pass

        return name

    async def take_a_hint(self, hint: Hint) -> List[NodeTransport]:
        instance_address = await self._resolve_instance(hint.service_name)
        return [NodeTransport(hint.protocol_mux, instance_address, hint.service_name)]

    async def _resolve_instance(self, service_name: str) -> str:
        """
        Look up the instance address of this service in aws.  It takes the first ec2 instance which has the service name
        as the ec2 tag: $aws_tag

        :param service_name: specify the service name to look up
        :return: an IP address associated with the ec2 instance discovered
        """
        logs.logger.debug(f"Performing reverse AWS name lookup for {service_name}")
        try:
            ec2 = boto3.client('ec2')
            filters = self._parse_filters(service_name)
            response = ec2.describe_instances(
                Filters=filters,
                MaxResults=5

            )
        except ClientError as e:
            _die(e)

        # parse name from response
        try:
            ip = response['Reservations'][0]['Instances'][0]['PrivateIpAddress']
        except (KeyError, IndexError) as e:
            print(colored(f"ec2 describe-instances response was insufficient for instance lookup", 'red'))
            print(colored(f"- {e}", 'yellow'))
            constants.PP.pprint(colored(filters, 'yellow'))
            constants.PP.pprint(colored(response, 'yellow'))
            raise e

        return ip

    def _parse_filters(self, service_name: str) -> List[dict]:
        """
        Generate AWS filters for the instance from service name and CLI args

        :param service_name: the service name to filter on
        :return:
        """
        filters = [{
            'Name': 'instance-state-name',
            'Values': ['running']
        }, {
            'Name': f"tag:{constants.ARGS.aws_service_name_tag}",
            'Values': [service_name]
        }]
        for tag, value in self.tag_filters.items():
            filters.append({
                'Name': f"tag:{tag}",
                'Values': [value]
            })
        return filters


def _die(e):
    print(colored('AWS boto3 Authentication Failed!  Please check your aws credentials, have you set AWS_PROFILE?',
                  'red'))
    print(colored(f"- {e}", 'yellow'))
    sys.exit(1)
