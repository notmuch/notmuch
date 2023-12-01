import setuptools
from _notmuch_config import *

with open(NOTMUCH_VERSION_FILE) as fp:
    VERSION = fp.read().strip()

setuptools.setup(
    name='notmuch2',
    version=VERSION,
    description='Pythonic bindings for the notmuch mail database using CFFI',
    author='Floris Bruynooghe',
    author_email='flub@devork.be',
    setup_requires=['cffi>=1.0.0'],
    install_requires=['cffi>=1.0.0'],
    packages=setuptools.find_packages(exclude=['tests']),
    cffi_modules=['notmuch2/_build.py:ffibuilder'],
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: GNU General Public License (GPL)',
        'Programming Language :: Python :: 3',
        'Topic :: Communications :: Email',
        'Topic :: Software Development :: Libraries',
    ],
)
