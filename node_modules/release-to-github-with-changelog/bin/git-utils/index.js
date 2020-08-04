#! /usr/bin/env node
const shell = require('shelljs');
const argv = require('yargs').argv;

const utils = require('../utils');

module.exports = {
  bumpPackageVersion,
  addAndCommitPackage,
  pushToMaster,
};

function bumpPackageVersion(bumpLevel) {
  shell.exec(`npm version --no-git-tag-version ${bumpLevel}`);
}

function addAndCommitPackage() {
  const version = utils.getVersionFromPackage();
  addAndCommitForVersion(version);
}

function addAndCommitForVersion(version) {
  shell.exec('git add package.json');
  shell.exec(`git commit package.json -m ${getReleaseMessage(version)}`);
}

function getReleaseMessage(version) {
  return `"Release ${version}"`;
}

function pushToMaster() {
  shell.exec('git push origin master');
}
