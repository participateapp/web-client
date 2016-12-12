# Participate!

An app for democratic decision-making.

Participate! aims to improve citizen participation in public issues, and participation in general at any organization interested in shared, democratic management.

It does so by focusing on concrete proposals rather than noisy and many times unproductive debate. 

A participant makes a proposal and gathers support for it. Other participants can collaborate on it if they support it in principle. Dissenters have to make a counter-proposal, and gather support for it as well, to be heard. 

Representation is ensured for participants who are less involved (be it for lack of time, inclination or of knowledge) through fluid delegation of support, in a [liquid democracy](https://en.wikipedia.org/wiki/Delegative_democracy).

Early stages of development. Participate! is open source and plans are to have a hosted version in the future as well.

## Stack

Elm front-end app talking to an [Elixir/Phoenix API](https://github.com/oliverbarnes/participate-api).


## Setup the client

### Prerequisites

- [Elm 0.17](http://elm-lang.org/)
- [modd 0.4](https://github.com/cortesi/modd) and [devd 0.7](https://github.com/cortesi/devd). They both come as a single executable, available for all major platforms [here](https://github.com/cortesi/modd/releases) and [here](https://github.com/cortesi/modd/releases).
- Optionally `unbuffer` for colorful compiler output. Included in the package [expect](https://www.nist.gov/services-resources/software/expect).
Available for [OSX](http://apple.stackexchange.com/questions/193138/to-install-unbuffer-in-osx) and [Linux](http://unix.stackexchange.com/a/25375) (e.g. [ArchLinux](https://www.archlinux.org/packages/extra/x86_64/expect/), [Debian](https://packages.debian.org/wheezy/expect-dev), [Ubuntu](http://packages.ubuntu.com/precise/expect-dev))


### Build

Begin by building and starting the [API](https://github.com/oliverbarnes/participate-api#setup).

Then to compile client's Elm sources and to start a development server:

```sh
$ modd
```

Access the web app from [http://devd.io:3000/](http://devd.io:3000/) (same as [http://localhost:3000/](http://localhost:3000/)).

More details can be found [on the wiki](https://github.com/oliverbarnes/participate/wiki/Development-Setup).


## Want to get involved?

We'll pair with you so you can get up to speed quickly, and we pair on features as well. 

[Shoot us an email](mailto:oliverbwork@gmail.com), we'll add you to our Slack channel to join the discussion and talk about next steps.

See the complete guide to contributing [here](CONTRIBUTING.md).

---

Participate was inspired by [LiquidFeedback](http://liquidfeedback.org), and the book published by its authors: [Principles of Liquid Feedback](http://principles.liquidfeedback.org)
