Contributing
============

Contributions are much appreciated!

You're welcome to submit [pull requests](https://github.com/participateapp/web-client/pulls), propose features and post bugs on [Github issues](https://github.com/participateapp/web-client/issues), and ask questions on our [Slack channel](https://participateapp.slack.com) (ping me for an invite [by email](mailto:oliverbwork@gmail.com) or on [twitter](http://twitter.com/digiberber)). 

Here are the steps for contributing code:

#### Fork the Project

Fork the [project on Github](https://github.com/participateapp/web-client) and check out your copy.

```
git clone https://github.com/contributor/web-client.git
cd participate
git remote add upstream https://github.com/participateapp/web-client.git
```

#### Create a Topic Branch

Make sure your fork is up-to-date and create a topic branch for your feature or bug fix.

```
git checkout master
git pull upstream master
git checkout -b my-feature-branch
```

If the feature or bug is related to an existing github issue, please prefix the branch with the issue number:

```
git checkout -b 75-fix-some-bug
```

#### Write Code

Implement your feature or bug fix. Please be sure to submit clean, well refactored code, as self-explanatory as possible.

#### Write Documentation

Document any new user-visible behavior in the [README](README.md).

Sometimes even with great code it's helpful to add some comments, specially with examples of usage. Add these inline.

#### Commit Changes

Make sure git knows your name and email address:

```
git config --global user.name "Your Name"
git config --global user.email "contributor@example.com"
```

Writing good commit logs is important. A commit log should describe what changed and why.

```
git add ...
git commit
```

#### Rebase

If you've been working on a change for a while, rebase with upstream/master.

#### Push

```
git push origin my-feature-branch
```

#### Make a Pull Request

Go to https://github.com/contributor/web-client and select your feature branch. Click the 'Pull Request' button and fill out the form. Pull requests are usually reviewed daily on weekdays.

If you're new to Pull Requests, check out the [Github docs](https://help.github.com/articles/using-pull-requests)


```
git fetch upstream
git rebase upstream/master
git push origin my-feature-branch -f
```

#### Be Patient

It's likely that your change will not be merged and that I will ask you to do more, or fix seemingly benign problems. Hang in there!
