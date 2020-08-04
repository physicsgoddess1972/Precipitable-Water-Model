#! /usr/bin/env node
const shell = require('shelljs');
const colors = require('colors');
const rp = require('request-promise');

const GITHUB_REPOS_URI = 'https://api.github.com/repos';

class GithubReleaseClient {
  constructor(repoFullname, token, branch) {
    this.repoFullname = repoFullname;
    this.token = token;
    this.branch = branch || 'master';
  }

  publishRelease(version, releaseTitle, releaseDescription, prerelease) {
    if (!version) {
      return Promise.reject('Please specify a version for the release to publish');
    }

    if (!releaseTitle) {
      return Promise.reject('Please specify a title for the release to publish');
    }

    const releaseResourceCmd = formGithubReleaseResource({
      version,
      prerelease,
      title: releaseTitle,
      description: releaseDescription,
      branch: this.branch,
    });

    const releaseUri = getReleasesUri(this.repoFullname);

    const options = {
      method: 'POST',
      uri: releaseUri,
      headers: {
        Accept: 'application/vnd.github.v3+json',
        'User-Agent': this.repoFullname.split('/')[0],
        Authorization: `token ${this.token}`,
        'Content-Type': 'application/json',
      },
      body: releaseResourceCmd,
      json: true,
    };

    console.log(`Publishing release ${version} against "${this.branch}" branch, to `.blue + releaseUri);

    return rp(options).catch(err => Promise.reject(err.response.body));
  }
}

function getReleasesUri(repoFullname) {
  return [GITHUB_REPOS_URI, repoFullname, 'releases'].join('/');
}

function formGithubReleaseResource({ version, title, description, branch, prerelease }) {
  const releaseResource = {
    tag_name: version.indexOf('v') < 0 ? `v${version}` : version,
    target_commitish: branch,
    name: title,
  };
  if (description) {
    releaseResource.body = description;
  }
  if (prerelease) {
    releaseResource.prerelease = prerelease;
  }
  return releaseResource;
}

function getPublishReleaseFunction(repoFullname, token, branch) {
  const githubReleaseClient = new GithubReleaseClient(repoFullname, token, branch);
  return (version, releaseTitle, releaseDescription, prerelease) => (
    githubReleaseClient.publishRelease(version, releaseTitle, releaseDescription, prerelease)
  );
}

module.exports = {
  getPublishReleaseFunction,
};
