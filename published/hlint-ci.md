# HLint on Travis/Appveyor

_Summary: Running HLint on your CI is now quick and easy._

I've always wanted to run [HLint](https://github.com/ndmitchell/hlint) on my continuous integration servers (specifically [Travis](https://travis-ci.org/) for Linux and [Appveyor](https://www.appveyor.com/) for Windows), to automatically detect code that could be improved. That has always been possible, and packages like [lens](https://github.com/ekmett/lens) and [freer-effects](https://github.com/IxpertaSolutions/freer-effects) have done so, but it was unpleasant for two reasons:

* Setting up a custom HLint settings file and applying these settings was a lot of upfront work.
* Building `hlint` on the CI server could be quite slow.

With HLint v2.0.4, both of these issues are addressed. I am now running HLint as standard for many of my projects. The two steps are outlined below.

**Setting up custom HLint settings**

Locally run `hlint . --default > .hlint.yaml` and it will generate a file which ignores all hints your project currently triggers. If you then run `hlint .` there will be no warnings, as the ignore settings will automatically be picked up. Check in `.hlint.yaml`.

Later, as a future step, you may wish to review your `.hlint.yaml` file and fix some of the warnings.

**Running HLint on the server**

There are now precompiled binaries at [GitHub](https://github.com/ndmitchell/hlint/releases/latest), along with scripts to download and execute them for each CI system. In both cases below, replace `ARGUMENTS` with your arguments to `hlint`, e.g. `.` to check the current directory.

On Travis, execute the following command:

    wget https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh -O - --quiet | sh -s ARGUMENTS

On Appveyor, add the following statements to `.appveyor.yml`:

    - set PATH=C:\Program Files\Git\mingw64\bin;%PATH%
    - curl -ohlint.bat -L https://raw.githubusercontent.com/ndmitchell/hlint/master/misc/appveyor.bat
    - hlint ARGUMENTS

Since these are precompiled binaries the additional time required to run HLint should be minimal.