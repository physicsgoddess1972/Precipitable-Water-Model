#! /usr/bin/env node
const fs = require('fs');

module.exports = {
  extractEnv,
  getRepoFullnameFromPackage,
  getVersionFromPackage,
};

function extractEnv(envFile) {
  const rawEnv = fs.readFileSync(envFile, 'utf8');
  const couples = rawEnv
  .replace(/'/g, '')
  .split('\n')
  .filter(couple => couple.indexOf('=') > -1);
  const env = {};
  couples.forEach(couple => {
    env[couple.split('=')[0]] = couple.split('=')[1];
  });
  return env;
}

function getRepoFullnameFromPackage() {
  const repoUrl = getPackageJson().repository.url;
  const urlAsArray = repoUrl.split('/');
  const indexOfGithubDomain = urlAsArray.findIndex(el => el.indexOf('github.com') > -1);

  const ownerName = urlAsArray[indexOfGithubDomain + 1];
  const repoName = urlAsArray[indexOfGithubDomain + 2].split('.')[0];

  return `${ownerName}/${repoName}`;
}

function getVersionFromPackage() {
  return getPackageJson().version;
}

function getPackageJson() {
  return JSON.parse(fs.readFileSync('package.json', 'utf8'));
}
