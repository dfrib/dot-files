# My Dot Files

This is intended to be a collection of the dot files I use to setup my work
environment in, primarily for Linux.

Initially, I only have my Emacs setup here.

## Emacs

I primarily work with C++ development in CMake projects under Git version
control, where more often than not the supporting tools are written in Python.
Thus, my Emacs configuration is inclined to primarily support C++ and Python
development, as well as supporting my recently started journey into literate
programming using the [Babel
Project](http://orgmode.org/worg/org-contrib/babel/intro.html),  based on
[org-mode](http://orgmode.org).

### Before DOOM

Prior to using the configuration tracked in this repository, I was using the
configuration covered in my
[`dfrib/emacs_setup`](https://github.com/dfrib/emacs_setup) repository, which
was a standard Emacs custom configuration centered around
[`cmake-ide`](https://github.com/atilaneves/cmake-ide) and
[`rtags`](https://github.com/Andersbakken/rtags) for setting up C++ IDE-like
Emacs features.

### The DOOM Era

I have since migrated to use a non-evil configuration of [DOOM
Emacs](https://github.com/hlissner/doom-emacs), moreover switching out
`cmake-ide` and `rtags` in favour of [`ccls`](https://github.com/MaskRay/ccls)
for my C++ IDE-like Emacs feature needs.

My non-evil configuration was initially forked from
[`UndeadKernel/emacs_doom_private`](https://github.com/UndeadKernel/emacs_doom_private),
see [the actual fork](https://github.com/dfrib/emacs_doom_private) for the
history prior to the creation of this repository.

Similarly to the setup instructions as the `dfrib/emacs_setup` repository, below
follows a summary of the pre-requisites for making use of my Emacs
configuration.

### Pre-requisites (Assuming Ubuntu)

#### CCLS

As is described in the [ccls
wiki](https://github.com/MaskRay/ccls/wiki/Getting-started), prior to building
the ccls language server, the are some pre-requisites.

##### Install GCC, version >= 7.3 (or higher, if required by CMake installation)

For details: see [How do I use the latest GCC on Ubuntu?](https://askubuntu.com/a/581497)

Add the [Toolchain Test Builds
PPA](https://launchpad.net/~ubuntu-toolchain-r/+archive/ubuntu/test):

```bash
$ sudo add-apt-repository ppa:ubuntu-toolchain-r/test
$ sudo apt-get update
```

Install e.g. GCC 8 and add the gcc-8 and g++8 alternatives to the system:

```bash
$ sudo apt-get install gcc-8 g++-8
$ sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-8 60 --slave /usr/bin/g++ g++ /usr/bin/g++-8
```

Use `sudo update-alternatives --config gcc` to configure which gcc to use as
default, in case you have other versions installed.

##### Install CMake, version >= 3.8

Make sure you have a recent GCC installed (see above), as building recent CMakes
places pre-requisites on GCC version.

The default version provided by e.g. Ubuntus package manager is usually ancient,
so remove that:

```bash
$ sudo apt remove cmake
$ sudo apt purge --auto-remove cmake
```

Download and unpack a recent version from [CMake's
webpage](http://www.cmake.org/download), noting that it is essential that you
keep the unpacked folder for later, to allow uninstalling the CMake you will now
install from source (see [this Q&A](https://askubuntu.com/a/942740)). E.g., to
download CMake 3.12.3 (Linux) to your (existing) `~opensource` directory and
thereafter unpack it:

```bash
$ cd ~/opensource
$ wget https://cmake.org/files/v3.12/cmake-3.12.3.tar.gz
$ tar -xzvf cmake-3.12.3.tar.gz
```

Install the extracted source:

```bash
$ cd ~/opensource/cmake-3.12.3
$ ./bootstrap
$ make -j4
$ sudo make install
```

This will be installed under ` /usr/local/bin/`, so you might want to close your
current terminal, and in a new terminal, try:

```bash
$ cmake --version # 3.12.3 ?
```

Recall from above: that if you'd like to uninstall this particular CMake
version, you need to return to the corresponding source and uninstall from
there:

```bash
$ cd ~/opensource/cmake-3.12.3
$ sudo make uninstall
```

##### Build CCLS

_(Mostly extracted from [CCLS: Getting
Started](https://github.com/MaskRay/ccls/wiki/Getting-started))_

Given that the steps above was successful, you are now ready to build the
language server. I usually place the project in `~opensource`.

```bash
$ cd ~/opensource
$ git clone https://github.com/MaskRay/ccls --depth=1
$ cd ccls
$ git submodule update --init
$ cmake -H. -BRelease # This downloads prebuilt clang+llvm from releases.llvm.org
$ cmake --build Release
```

### Installing DOOM Emacs

#### Personal settings

Clone this repository and place a symlink `~/.doom.d` to the `emacs/.doom.d`
folder of this repository.

E.g.:

```bash
$ cd ~/opensource
$ git clone https://github.com/dfrib/dot-files.git
$ ln -s ~/opensource/dot-files/emacs/.doom.d ~/.doom.d
```

#### DOOM Emacs

Taking parts of _Quick Start_ from `develop` branch of
[`hlissner/doom-emacs`](https://github.com/hlissner/doom-emacs/blob/develop/README.md)

```bash
$ git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
$ cd ~/.emacs.d
$ git checkout develop
$ ./bin/doom quickstart
```

Every time you modify your `~/.doom.em/init.el`, make sure to run:

```bash
$ ~/.emacs.d/bin/doom refresh
```

That should be it!
