# v1.2.4

## Fix build

# v1.2.3

## Allow multiple newlines between tagName and releaseTitle to account for Prettier rule

# v1.2.2

## Improve error details when checks fail

# v1.2.1

## Update build and dependencies

Update build to circle v2 as v1 is deprecated.
Update dependencies to get rid of some vulnerabilities

# v1.2.0

## Add support for pre-releases

Adding a version like "v1.2.3-beta.1" will publish a beta pre-release.

# v1.1.5

## Remove dependency on jq

Only use `shelljs` for `exit` and `exec`

# v1.1.4

## Allow several digits versions parts

Fixes issue 14

# v1.1.3

## Improve README

# v1.1.2

## Fix v1.1.0 and v1.1.1

The repo full name was including the .git suffix by mistake.
So the release push API was called with wrong repo name /shrug.

# v1.1.1

## Bump version to fix CI

# v1.1.0

## Get Github repository full name from repository url

Before, it was required to have the full name under `repository.fullname` in your `package.json`.

# v1.0.0

## Separator not needed anymore

Previously, a `<!-- -->` separator was needed in the CHANGELOG.md.
Now, every item is identified by its structure.

### Example

```
# v1.0.0
## Release title
### Some markdown notes
About the release
```

# v0.7.0

## Fail job when badly formatted CHANGELOG

Previously, a trailing separator could fail the job for an unexpected reason.
Now the `pre-release-checks` also checks every CHANGELOG item is well formatted.

<!-- -->

# v0.6.1

## Fix version discrepancies

Between npm and github

<!-- -->

# v0.6.0

## Add pre-release-checks script

To make sure a branch or PR has `package.json` and `CHANGELOG.md` in sync, you can now add this script to your tests:

```
"scripts": {
  "test": "mocha ... && release-to-github-with-changelog-pre-release-checks",
  "release-to-github": "release-to-github-with-changelog --branch=releases",
},
```

<!-- -->

# v0.5.5

## Fix

<!-- -->

# v0.5.4

## Improve logging

Refactored tests

<!-- -->

# v0.5.3

## Bump for test

<!-- -->

# v0.5.2

## Fix tests

Mocha was calling the main file

<!-- -->

# v0.5.1

## Fix build

Bump version

<!-- -->

# v0.5.0

## Allow to specify branch name

### Usage

Pass the --branch=myBranch to the command

<!-- -->

# v0.4.1

## Small fixes

<!-- -->

# v0.4.0

## Big refactor

<!-- -->

# v0.3.1

## Remove unused imports

<!-- -->

# v0.3.0

## Add better validation on repo fullname

And log release uri as info

<!-- -->

# v0.2.5

## Add better validation on presence of repository fullname property

Test if package.json has correct github repo fullname

<!-- -->

# v0.2.4

## Fix installation issue

bin script wasn not referencing the bin folder

<!-- -->

# v0.2.3

## Add README.md

<!-- -->

# v0.2.2

## Use CircleCI for auto release

<!-- -->

# v0.2.1

## Make tests pass

<!-- -->

# v0.2.0

## Use env variable for Github token

You should now expose the github token as GITHUB_TOKEN in env variables

<!-- -->

# v0.1.0

## Different changelog separator

### What it does

Lalala

<!-- -->

# v0.0.2

## Alpha version fix

### What it does

Fixes the description bug

### Caveats

maybe a few remaining

<!-- -->

# v0.0.1

## Alpha version

### What it does

not much

### Caveats

a lot
