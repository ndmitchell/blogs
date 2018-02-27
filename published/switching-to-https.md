# Switching to HTTPS

_Summary: All my domains are now on HTTPS. In this post I describe how I did it._

It's quite clear everyone should be moving their domains to HTTPS, or [face the consequences](https://developers.google.com/web/updates/2016/10/avoid-not-secure-warn). I have recently converted three domains to HTTPS - two static sites and one Haskell server. Converting was mostly a case of finding the right how-to guide and following it, so in this post I'll link to the "right" guides.

## Static Websites

I have static domains at [shakebuild.com](https://shakebuild.com/) and [ndmitchell.com](https://ndmitchell.com), both of which follow a similar pattern, so I'll focus on the Shake website. The steps are:

* **Get a domain name**: I bought a domain name from [Soho UK](https://www.soho-uk.com/), who later sold up and became Cloud Mega. I've been using them for websites for a very long time, and never had any complaints.
* **Write some content**: My static websites are based of source material that is then converted via custom scripts to generate the final website. For Shake, the source is [Markdown files](https://github.com/ndmitchell/shake/tree/master/docs) and the converter is [a Haskell program](https://github.com/ndmitchell/shake/tree/master/website). In the case of Shake, I use the [`markdown` package](https://hackage.haskell.org/package/markdown-0.1.16/docs/Text-Markdown.html) with custom tricks like hyperlinking all identifiers (see [these code samples](https://shakebuild.com/manual)). After running the program on the Markdown files I have HTML/CSS that can be served directly.
* **Serve the content**: I host and serve the content using [GitHub Pages](https://pages.github.com/), which lets you either serve content off the branch `gh-pages` or a separate GitHub repo - I use the latter option. I then use the [custom domain name](https://help.github.com/articles/using-a-custom-domain-with-github-pages/) feature to make requests to `shakebuild.com` serve from GitHub Pages over HTTP.
* **Serve with HTTPS**: The previous steps get us an HTTP website, but last weekend I did the work to get to HTTPS. I followed [these instructions](https://www.goyllo.com/cloudflare-ssl-for-github-pages/), which use [Cloudflare](https://www.cloudflare.com/) as an intermediary - serving over HTTPS and providing a cache. I have configured things to always redirect away from the `www` and always use HTTPS. The only minor hiccup was the HTTPS certification for Shake took about 3 days to initialise (it should take less than 24 hours, my other domain took 15 minutes) - but it went away on its own.
* **Collect email**: The final step was to get email to the domains working - in general I'd prefer people email me directly at Gmail, but it's always good for email to work. I used [these instructions](https://renzo.lucioni.xyz/mail-forwarding-with-mailgun/), which use [Mailgun](https://www.mailgun.com/) to collect and forward emails. The only difficulty is that sending Gmail emails to yourself via a custom domain leaves the email in the Sent mail with no indication it was delivered - I had to test using a different email account.

With that, we have a static website served over HTTPS. It's quite remarkable that such a pipeline can be built using free services.

## Dynamic Website

I maintain the [hoogle.haskell.org](https://hoogle.haskell.org/) server which provides a search engine for [Haskell libraries](https://www.haskell.org/). This website is dynamic, executing Haskell code to return suitable results for each search.

* **Write the program**: I wrote [Hoogle](https://github.com/ndmitchell/hoogle) over the last 14 years, and when run as `hoogle server` it spawns a web server which can serve requests, using the [Warp package](https://hackage.haskell.org/package/warp) to do the actual serving.
* **Configure the server**: The `hoogle.haskell.org` server is kindly provided by the [Haskell Infrastructure Committee](https://wiki.haskell.org/Haskell.org_infrastructure), where I have a VM which runs Hoogle. My setup instructions for that server are [in the Hoogle repo](https://github.com/ndmitchell/hoogle/blob/master/docs/Haskell.org.md). Of note, I forward port 80 to 8080, allowing me to serve HTTP pages with a non-root program.
* **Serve static content over CDN**: The static content of Hoogle (images, CSS files) could be served up by the normal server, but it's just one small server in one single location, so I make things go faster by sending most static requests to [Raw GitHack](https://raw.githack.com/), which itself is just a wrapper around [Cloudflare](https://www.cloudflare.com/).
* **Obtain certificates**: To serve over HTTPS you need certificates that prove you own the domain. I got the certificates from [Let's Encrypt](https://letsencrypt.org/getting-started/), using the [Certbot](https://certbot.eff.org/) client. Since I run a custom server I opted for the Standalone challenge (which spawns a web server on your box), over HTTP, serving on port 8080 to account for the redirection I had put in place. Unfortunately, generating the certificates required taking Hoogle down briefly.
* **Serving over HTTPS**: Fortunately a [PR was submitted](https://github.com/ndmitchell/hoogle/pull/156) to Hoogle some time ago allowing users to pass a certificate at startup and serve Hoogle over HTTPS. I passed the certificates obtained in the previous step, and spawned Hoogle on 8443 (which 443 redirected too), giving me an HTTPS server.
* **Redirecting HTTP traffic**: For the static websites redirecting HTTP traffic to HTTPS was as simple as checking a box on Cloudflare. For my own server I needed to run a server on port 8080 that did the redirect. I found the Haskell program [rdr2tls](https://github.com/fpco/rdr2tls) which is small, simple, and works very well.
* **Renewing the certificate**: The Let's Encrypt serve expires every 90 days, so will need renewing. I know the approximate steps, but currently am intending to manually renew the certificate.

Switching Hoogle to HTTPS was fairly painless.
