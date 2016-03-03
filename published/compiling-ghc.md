# Compiling GHC on Windows

_Summary: How to compile GHC on Windows using Stack and the new Shake-based GHC build system._

Here are a list of instructions to compile GHC, from source, on Windows. I tested these instructions on a clean machine using the [free Windows 10 VirtualBox image](https://dev.windows.com/en-us/microsoft-edge/tools/vms/windows/) (I bumped the VM CPUs to 4, and RAM to 4096Mb).

The first step is to [install Stack](https://www.stackage.org/stack/windows-x86_64-installer) (I just accepted all the defaults), then open a command prompt and run:

	stack setup
	stack install happy alex
	stack exec -- pacman -S gcc binutils git automake-wrapper tar make patch autoconf --noconfirm
	stack exec -- git clone --recursive git://git.haskell.org/ghc.git
	cd ghc
	stack exec -- git clone git://github.com/snowleopard/shaking-up-ghc shake-build
	stack build --stack-yaml=shake-build/stack.yaml --only-dependencies
	stack exec -- perl boot
	stack exec -- bash configure --enable-tarballs-autodownload
	stack exec --stack-yaml=shake-build/stack.yaml -- shake-build/build.bat -j

The entire process (after the VM has downloaded) takes a bit less than an hour. These steps use the Stack supplied tools (MinGW, Git), and the new [Shake-based build system](https://github.com/snowleopard/shaking-up-ghc). The hope is that by using the isolation Stack provides, combined with the portability improvements from writing the build system in Haskell, these instructions will work robustly on many Windows machines.

I have not tried these instructions on other platforms, but suspect that removing the `pacman` line might be sufficient to get it to work.
