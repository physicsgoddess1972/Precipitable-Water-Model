# release-to-github-with-changelog
[![CircleCI](https://circleci.com/gh/adrienDog/release-to-github-with-changelog.svg?style=shield&circle-token=:9f1c74e21caa562c5e012b5a781d0ce4d15812d2)](https://circleci.com/gh/adrienDog/release-t--github-with-changelog)

[![npm version](https://badge.fury.io/js/release-to-github-with-changelog.svg)](https://badge.fury.io/js/release-to-github-with-changelog)

*Node v6.9.1 is required*

## Goal
Keep the released npm package in sync with the GitHub repo master branch:
- the last `CHANGELOG.md` item is in sync with the last release on Github, with corresponding version tag
- the `package.json` version is in sync with the `master` branch version tag

![alt tag](img/changelog_releases_sync.png)

## Usage
The sources of truth are `CHANGELOG.md` and `package.json`.
Your interface is your `CHANGELOG.md`.
### CHANGELOG.md
Every `CHANGELOG.md` item should represent a release note of the version it describes:
```
# v0.2.0 //version tag
## The release title
// The release description in markdown

# v0.1.9
...
```
#### Full example:
```
# v0.2.0
## We can fly
Great news! The machine can now also fly!

# v0.1.0
## We can move
### What the machine does
It can move, a bit slowly but still it moves.

//...
```
### Export a GITHUB_TOKEN env variable
Either locally with `export GITHUB_TOKEN=$yourToken` or in your CI tool settings (see CircleCI example).

### Example of package.json
Don't forget the `repository.url`. It will be parsed to extract the repository full name (`myOrg/myRepo` in this example).
#### Test tool
You can include a check of your `CHANGELOG.md` format in your test command by using the provided `release-to-github-with-changelog-pre-release-checks` command.
```
{
  "version": "0.2.0",
  "respository": {
    "type": "git",
    "url": "git+https://github.com/myOrg/myRepo.git"
  },
  "files": ["dist"],
  "scripts": {
    "build": // build dist files
    "release": "npm publish && npm run release-to-github-with-changelog",
    "release": "npm publish && npm run release-to-github-with-changelog --branch=releases", // optional branch name
    "test": "release-to-github-with-changelog-pre-release-checks && karma start"
  }
}
```
## CI example
`circle.yml`
```
machine:
  node:
    version: 4.1.0
dependencies:
  pre:
    - echo -e "$NPM_USER\n$NPM_PASS\n$NPM_EMAIL" | npm login
deployment:
  production:
    branch: master
    commands:
      - npm run release
```
