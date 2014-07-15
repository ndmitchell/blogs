# Shake Profiling

There are lots of ways to profile in the new version. Including the stdout report, the new profiling tutorial, the Chrome bit and a JSON dump. Plus there is always timing and GHC Profiling.

For info, you can use --timings and --profile=- to get some visibility
on what is taking the .2s/.45s. It prints maybe 20 lines on stdout
with quite a lot of info. There is always GHC profiling as well.

Use --report=file.html, and see
https://cdn.rawgit.com/ndmitchell/shake/35fbe03c8d3bafeae17b58af89497ff3fdd54b22/html/demo.html
for an example, plus how to interpret a profile report.
