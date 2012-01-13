# libirc [![Build Status](https://secure.travis-ci.org/rfc1459/libirc.png)](http://travis-ci.org/rfc1459/libirc)

libirc is an IRC utility library for Erlang.

**License:** libirc is licensed under the 2-clause BSD license.

**Feedback:** morpheus@azzurra.org

## Features

* Mostly [RFC2812][]-compliant parser, with some requirements relaxed
* Parser is role-agnostic, it can be used both in client or server contexts
* [RFC1459 case mapping][rfc1459] functions are implemented as NIFs
* Still needs a lot of work :-)

## Usage Examples

```erlang
1> libirc:parse("PRIVMSG #services :hello, there!").
[{prefix,<<>>},
 {command,"PRIVMSG"},
 {args,[<<"#services">>,<<"hello, there!">>]}]
2> libirc:to_rfc1459_upper("~some string}").
"^SOME STRING]"
3> libirc:to_rfc1459_lower(<<"A binary TOO!">>).
<<"a binary too!">>
```

## Bug reports

As usual, file an [issue][] on GitHub.

[RFC2812]: http://tools.ietf.org/html/rfc2812#section-2.3.1
[rfc1459]: http://tools.ietf.org/html/rfc1459#section-2.2
[issue]: https://github.com/rfc1459/libirc/issues/new
