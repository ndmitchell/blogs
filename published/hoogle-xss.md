# Hoogle XSS Vulnerability

_Summary: Hoogle 5.0.17.6 and below have an XSS vulnerability, fixed in later versions._

On Friday afternoon I got an email from [Alexander Gugel](https://github.com/alexanderGugel) with the subject line "Non-persistent XSS vulnerability on hoogle.haskell.org" - never a good thing to get. He had found that [Hoogle](https://hoogle.haskell.org/) was echoing the user search string back into the page, meaning that if you searched for `%27"><marquee style` you could make all the results scroll past in a disturbingly hypnotic manner. Oh dear!

#### Step 1: Fix the website

The first concern was to fix the website. While there aren't any cookies stored by Hoogle, and there are no logon forms or similar, the [Project Zero blog](https://googleprojectzero.blogspot.com/) has taught me that given the tiniest chink, everything can be broken. Fortunately, Alex emailed me using the [email address on my webpage](https://ndmitchell.com/), described the problem, and provided a 3 line diff that escaped all the problematic variables. I applied this fix and pushed a new version to [hoogle.haskell.org](https://hoogle.haskell.org/).

#### Step 2: Use the type system

Like any good Haskeller, my first thought on encountering a bug is to use the type system to prevent it by construction. The problem boils down to taking user input and splicing it into an HTML page. My initial fix was to introduce a type [`Taint`](https://github.com/ndmitchell/hoogle/blob/v5.0.17.7/src/General/Web.hs#L39):

```
newtype Taint a = Taint a

escapeUntaint :: Taint String -> String
escapeUntaint (Taint x) = escapeHTML x
```

The idea is that instead of the query parameters to the web page being `String`'s that can be carelessly spliced into the output, they were `Taint String` values whose only real unwrapping function involves escaping any HTML they may contain. Furthermore, `Taint` can have instances for `Monad` etc, meaning you can work on tainted values, but the result will always remain tainted.

Using this approach uncovered no additional problems, but gave me much more confidence there weren't any I just hadn't found.

#### Step 3: Make a release

At this point I made a release of Hoogle 5.0.17.7. This version has no known XSS issues with it.

#### Step 4: Switch to blaze-html

While `Taint` is an effective tool for some domains, the real problem for Hoogle was that I was building up HTML values using `String` - making it way too easy to create invalid HTML, and providing an easy attack vector. The next change was to switch to [`blaze-html`](https://hackage.haskell.org/package/blaze-html), which uses strong typing to ensure the HTML is always valid. Instead of having to call `escapeHTML` to turn bad `String` into good `String`, I instead used `H.string` to turn bad `String` into good `Markup`. For the rare case where there genuinely was `String` that contained HTML for good reasons I used `H.preEscapedString`, making the "don't escape" explicit and longer, and the "do escape" the default - a much safer default.

#### Step 5: Use Content Security Policy headers

There are a whole suite of headers that can be returned by the server to opt in to additional checking, known as [CSP headers](https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP). These headers can ban inline script, detect XSS attacks, avoid confusion with MIME types, avoid `http` downgrade attacks and more. Thanks to [Gary Verhaegen](https://github.com/gaverhae) many of these are now [applied to Hoogle](https://github.com/ndmitchell/hoogle/blob/v5.0.17.8/src/General/Web.hs#L85-L146), meaning that even if my code is wrong, the chances of it causing any damange (even just hypnotic scrolling) are much reduced.

#### Step 6: Relax

Hoogle 5.0.17.8 has all the security fixes listed and is deployed to [hoogle.haskell.org](https://hoogle.haskell.org/). Hopefully no more security issues for a while!

_Many thanks to [Alexander Gugel](https://github.com/alexanderGugel) for the responsible disclosure, and to [Gary Verhaegen](https://github.com/gaverhae) for his work on CSP headers._
