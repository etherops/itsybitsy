import pytest


class TestNode:
    # is_crawlable()
    @pytest.mark.parametrize('warnings,errors', [(['FOO_WARN'], []), ([], ['FOO_ERR'])])
    def test_is_crawlable_case_warnings_errors(self, warnings, errors, node_fixture):
        """not crawlable with either errors or warnings"""
        # arrange
        node_fixture.warnings = warnings
        node_fixture.errors = errors

        # act/assert
        assert not node_fixture.is_crawlable(0)

    def test_is_crawlable_case_skip_service_name(self, node_fixture, mocker):
        """not crawlable if service name is skipped"""
        # arrange
        node_fixture.service_name = 'dummy'
        mocker.patch('itsybitsy.node.charlotte_web.skip', return_value=True)

        # act/assert
        assert not node_fixture.is_crawlable(0)

    def test_is_crawlable_case_skip_nonblocking_grandchildren(self, cli_args_mock, node_fixture, mocker):
        """nonblocking grandchild not crawlable if --skip-nonblocking-grandchildren is specified"""
        # arrange
        cli_args_mock.skip_nonblocking_grandchildren = True
        node_fixture.service_name = 'dummy'
        mocker.patch('itsybitsy.node.charlotte_web.skip', return_value=False)
        node_fixture.protocol = mocker.patch('itsybitsy.charlotte_web.Protocol', autospec=True, blocking=False)

        # act/assert
        assert not node_fixture.is_crawlable(2)

    def test_is_crawlable_case_happy_path(self, cli_args_mock, node_fixture, mocker):
        """nonblocking grandchild not crawlable if --skip-nonblocking-grandchildren is specified"""
        # arrange
        cli_args_mock.skip_nonblocking_grandchildren = False
        node_fixture.service_name = 'dummy'
        mocker.patch('itsybitsy.node.charlotte_web.skip', return_value=False)

        # act/assert
        assert node_fixture.is_crawlable(0)

    # is_excluded()
    def test_is_excluded_case_disabled_provider(self, cli_args_mock, node_fixture):
        # arrange
        disabled_provider = 'disabled_provider'
        cli_args_mock.disable_providers = [disabled_provider]
        node_fixture.provider = disabled_provider

        # act/assert
        assert node_fixture.is_excluded(0)

    @pytest.mark.parametrize('depth,expected_excluded', [(0, False), (1, False), (2, True)])
    def test_is_excluded_case_skip_nonblocking_grandchildren(self, cli_args_mock, depth, expected_excluded,
                                                             node_fixture, mocker):
        # arrange
        cli_args_mock.skip_nonblocking_grandchildren = True
        node_fixture.protocol = mocker.patch('itsybitsy.charlotte_web.Protocol', autospec=True, blocking=False)

        # act/assert
        assert node_fixture.is_excluded(depth) == expected_excluded

    # is_database()
    @pytest.mark.parametrize('port', ['3306', '5432', '9160'])
    def test_is_database_case_database_ports(self, node_fixture, port, mocker):
        """Node is a database from it's port/mux(DB port)"""
        # arrange
        node_fixture.protocol = mocker.patch('itsybitsy.node.charlotte_web.Protocol', is_database=False)
        node_fixture.protocol_mux = port

        # act/assert
        assert node_fixture.is_database()

    @pytest.mark.parametrize('port', ['11211', '6379'])
    def test_is_database_case_cache_ports(self, port, node_fixture, mocker):
        """Node is a database from it's port/mux(cache port, cache treated as DB here)"""
        # arrange
        node_fixture.protocol = mocker.patch('itsybitsy.node.charlotte_web.Protocol', is_database=True)
        node_fixture.protocol_mux = port

        # act/assert
        assert node_fixture.is_database()

    @pytest.mark.parametrize('port', ['80', '443', '21', '8080', '8443'])
    def test_is_database_case_nondatabase_ports(self, port, node_fixture, mocker):
        """Node is not a database from non DB ports"""
        # arrange
        node_fixture.protocol = mocker.patch('itsybitsy.node.charlotte_web.Protocol', is_database=False)
        node_fixture.protocol_mux = port

        # act/assert
        assert not node_fixture.is_database()

    def test_is_database_case_databasey_protocol(self, node_fixture, mocker):
        """Node is a database because it's protocol is defined as such"""
        # arrange
        node_fixture.protocol = mocker.patch('itsybitsy.node.charlotte_web.Protocol', is_database=True)

        # act/assert
        assert node_fixture.is_database()

    # crawl_complete()
    def test_crawl_complete_case_name_lookup_incomplete(self, cli_args_mock, node_fixture, mocker):
        """Crawl is not complete if name lookup is incomplete"""
        # arrange
        cli_args_mock.skip_nonblocking_grandchildren = False
        node_fixture.name_lookup_complete = mocker.Mock(return_value=False)

        # act/assert
        assert not node_fixture.crawl_complete(depth=0)

    def test_crawl_complete_case_max_depth_reached(self, cli_args_mock, node_fixture, mocker):
        """Crawl is complete when max_depth is reached"""
        # arrange
        node_fixture.name_lookup_complete = mocker.Mock(return_value=True)
        cli_args_mock.max_depth = 42

        # act/assert
        assert node_fixture.crawl_complete(depth=42)

    def test_crawl_complete_case_skip_service_name(self, node_fixture, mocker):
        """Crawl is complete if the service is configured to not be crawled"""
        # arrange
        node_fixture.name_lookup_complete = mocker.Mock(return_value=True)
        node_fixture.service_name = 'stub'
        skip = mocker.patch('itsybitsy.node.charlotte_web.skip', return_value=True)

        # act/assert
        assert node_fixture.crawl_complete(0)
        skip.assert_called_with('stub')

    def test_crawl_complete_case_skip_nonblocking_grandchildren(self, cli_args_mock, node_fixture, mocker):
        """Crawl is complete if the service is nonblocking and a grandchild, if respective CLI arg specified"""
        # arrange
        cli_args_mock.skip_nonblocking_grandchildren = True
        node_fixture.name_lookup_complete = mocker.Mock(return_value=False)
        node_fixture.protocol = mocker.patch('itsybitsy.charlotte_web.Protocol', autospec=True, blocking=False)

        # act/assert
        assert node_fixture.crawl_complete(2)

    @pytest.mark.parametrize('children,expected', [(None, False), ({}, True), ({'DUMMY': 'DUMMY'}, True)])
    def test_crawl_complete_case_children(self, children, expected, node_fixture, mocker):
        """Crawl is complete when children dict is present.  Here `None` has a different meaning than `{}`"""
        # arrange
        node_fixture.name_lookup_complete = mocker.Mock(return_value=True)
        node_fixture.children = children

        # act/assert
        assert node_fixture.crawl_complete(0) == expected

    @pytest.mark.parametrize('errors,expected', [({}, False), ({'DUMMY': 'DUMMY'}, True)])
    def test_crawl_complete_case_errors(self, errors, expected, node_fixture, mocker):
        """Crawl is complete if errors have been encountered"""
        # arrange
        node_fixture.name_lookup_complete = mocker.Mock(return_value=True)
        node_fixture.errors = errors

        # act/assert
        assert node_fixture.crawl_complete(0) == expected

    # name_lookup_complete()
    def test_name_lookup_complete_case_incomplete(self, node_fixture):
        """Name lookup is incomplete with no name and no errors"""
        # arrange
        node_fixture.service_name = None
        node_fixture.errors = {}

        # act/assert
        assert not node_fixture.name_lookup_complete()

    def test_name_lookup_complete_case_warnings(self, node_fixture):
        """Name lookup complete if we have warnings"""
        # arrange
        node_fixture.service_name = None
        node_fixture.errors = {}
        node_fixture.warnings = {'STUB': True}

        # act/assert
        assert not node_fixture.name_lookup_complete()

    def test_name_lookup_complete_case_service_name(self, node_fixture):
        """Name lookup complete if we have a name!"""
        # arrange
        node_fixture.service_name = 'stub'
        node_fixture.errors = {}

        # act/assert
        assert node_fixture.name_lookup_complete()

    def test_name_lookup_complete_case_errors(self, node_fixture):
        """Name lookup complete if we have errors"""
        # arrange
        node_fixture.service_name = None
        node_fixture.errors = {'STUB': None}

        # act/assert
        assert node_fixture.name_lookup_complete()
