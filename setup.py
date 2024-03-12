import re
from setuptools import setup

version = re.search(
    r'^__version__\s*=\s*"(.*)"',
    open('itsybitsy/itsybitsy.py').read(),
    re.M
).group(1)

with open("README.md", "r") as fh:
    long_description = fh.read()

setup(
    name="itsybitsy",
    version=version,
    author="etherops",
    author_email="patrick@life360.com",
    description="It's like a web-crawler, but for microservices",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/life360/itsybitsy",
    packages=['itsybitsy', 'itsybitsy.plugins'],
    entry_points={
        "console_scripts": ['itsybitsy = itsybitsy.itsybitsy:main']
    },
    install_requires=[
        'asyncssh~=2.14',
        'boto3~=1.16',
        'configargparse~=1.2',
        'coolname~=2.0',
        'faker>=4.1',
        'kubernetes~=11.0',
        'paramiko~=3.4',
        'pyyaml~=6.0',
        'graphviz>=0.13',
        'termcolor~=2.0',
    ],
    extras_require={
        'test': [
            'prospector~=1.2',
            'pytest~=6.2',
            'pytest-asyncio~=0.14',
            'pytest-cov~=2.10',
            'pytest-mock~=3.4'
        ]
    },
    setup_requires=[
        'wheel>=0.36'
    ],
    classifiers=[
        "Programming Language :: Python :: 3.8",
        "License :: OSI Approved :: Apache Software License     ",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.8',
)
