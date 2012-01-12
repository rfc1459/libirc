# libirc [![Build Status](https://secure.travis-ci.org/rfc1459/libirc.png)](http://travis-ci.org/rfc1459/libirc)

libirc is an IRC parsing library for Erlang.

**License:** libirc is licensed under the 2-clause BSD license.

**Feedback:** morpheus@azzurra.org

## Features

* Mostly [RFC2812][]-compliant, with some requirements relaxed
* Can be used both as a server-side parser or client-side parser
* Still needs a lot of work :-)

## Usage Examples

```erlang
1> libirc:parse("PRIVMSG #services :hello, there!").
[{prefix,<<>>},
 {command,"PRIVMSG"},
 {args,[<<"#services">>,<<"hello, there!">>]}]
```

## Bug reports

As usual, file an [issue][] on GitHub.

[RFC2812]: http://tools.ietf.org/html/rfc2812#section-2.3.1
[issue]: https://github.com/rfc1459/libirc/issues/new
