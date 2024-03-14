# Itsy Bitsy Spider

Configure charlotte, give it a seed node, and it crawls the graph/tree of your services oriented architecture.

![](./assets/demo.gif)

## Prerequisites
* python 3.8 available at `/usr/local/env python3`
  * python >= 3.7 was chosen in order to use python dataclasses
  * python >= 3.8 was chosen in order to use unittest.mock AsyncMock
* dot/graphviz binaries installed in system PATH (e.g. `brew install graphviz`)

## Configure itsybitsy in 7 easy steps!
1. Review the example project in [examples/example-project](examples/example-project)
1. Start a new project / empty folder
    1. `mkdir myitsybitsy && cd myitsybitsy`
    1. `venv'
    1. `echo "git+ssh://git@github.com/life360/itsybitsy.git#egg=itsybitsy" > requirements.txt`
    1. `pip install -r requirements.txt`
1. Configure charlotte - the configuration engine with which you will describe your service graph to itsybitsy
    1. `mkdir charlotte.d`
    1. Create a/several `...CrawlStrategy.yaml` file(s).
        1. Please see [examples/ExampleSSHCrawlStrategy.yaml](examples/ExampleSSHCrawlStrategy.yaml) for example/documentation.
    2. Crate `web.yaml` file  
        1. "Providers", "skips" , and "Hints" are all defined in [examples/web.yaml](examples/web.yaml). 
1. Run `itsybitsy --help` for all available commands and `itsybitsy spider --help` and `itsybitsy render --help` for command specific configuration.
1. Disable builtin provider with the argument `--disable-providers ssh aws k8s`
1. Set any configurations which are known to be required for every run in `spider.conf` see [./examples/spider.conf.example](./examples/spider.conf.example)
  1. Hint: `spider.conf` is always inherited, but you can create different profiles such as `spider.prod.conf` and reference them with the `--profile` arg

Note: unlike the `spider` command, `render` is written to stand alone and parse the default json file in `outputs/.lastrun.json` it requires no arguments by default.

## Use
#### 1 Run in `spider` mode:

```
$ itsybitsy spider -s ssh:$SEED_IP
foo [seed] (10.1.0.26)
 |--HAP--? {ERR:TIMEOUT} UNKNOWN [port:80] (some-unreachable.host.local)
 |--HAP--> mono [port:80/443] (10.0.0.123)
 |          |--NSQ--? {ERR:NULL_ADDRESS} UNKNOWN [some-multiplexor] (None)
```


#### 3 Run in `render` mode
It will by default render the "last run" automatically dumped to .lastrun.json.  Or you can pass in `-f` to load a specific file.  The default renderer is `ascii` unless a different render is passed in, as in `--output graphviz`

``` 
$ itsybitsy render
foo [seed] (10.1.0.26)
 |--HAP--? {ERR:TIMEOUT} UNKNOWN [port:80] (some-unreachable.host.local)
 |--HAP--> mono [port:80/443] (10.0.0.123)
 |          |--NSQ--? {ERR:NULL_ADDRESS} UNKNOWN [some-multiplexor] (None)
...
```

## Help
```
./itsybitsy --help
```


## Contributing

### Unit Tests
#### Design Choices
* `pytest` is used instead of `unittest` for more succinct test/reporting
* `pytest-mock` is used for mocks, so you will see the `mocker` fixtures passed around magically for mocking. It is used in combination with the parameter `new=sentinel.devnull` in order to not pass the patched mock to the test function. 
* Arrange, Act, Assert test style is used
* mocks are preferred for dependencies over production objects
* tests of objects are organized into a TestClass named after the object 1) for organization and 2) so that IDEs can find the test/subject relationship.
* tests are named in the following format: `"test_{name_of_function}_case_{description_of_case}"`
* the string 'dummy' is used to indicate that a value is assigned solely to meet argument requirements, but unused otherwise
* fixtures are placed in conftest.py only if they are use in common between test packages, otherwise please keep them in the package they are used in
* the idiomatic references 'foo', 'bar', 'baz', 'buz', etc are used when passing stub values around.  if you choose not to follow precedent:  please use something obvious like 'stub1', 'stub2', etc

#### Run tests
```
pytest
```

#### Run coverage
```
pytest --cov=water_spout tests
```

### Static Code Analysis
```
prospector --profile .prospector.yaml 
```

