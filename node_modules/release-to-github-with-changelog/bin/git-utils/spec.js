describe('git-utils', () => {

  const chai = require('chai');
  const assert = chai.assert;
  const sinon = require('sinon');
  const proxyquire =  require('proxyquire');

  const VERSION = '0.0.1';

  const shellStub = {
    exec: sinon.spy(),
  };
  const utilsStub = {
    getVersionFromPackage: () => VERSION,
  };

  // when no overrides are specified, path.extname behaves normally
  const gitUtils = proxyquire('.', {
    shelljs: shellStub,
    '../utils': utilsStub,
  });

  describe('bumpPackageVersion', () => {
    it('should make correct shell query', () => {
      gitUtils.bumpPackageVersion(VERSION);

      assert(shellStub.exec.calledWith(`npm version --no-git-tag-version ${VERSION}`));
    });
  });

  describe('addAndCommitPackage', () => {
    it('should add package.json', () => {
      gitUtils.addAndCommitPackage(VERSION);

      assert(shellStub.exec.calledWith('git add package.json'));
    });

    it('should commit package.json with correct message', () => {
      gitUtils.addAndCommitPackage(VERSION);

      const expectedMessage = `"Release ${VERSION}"`;
      assert(shellStub.exec.lastCall.calledWith(`git commit package.json -m ${expectedMessage}`));
    });
  });

  describe('pushToMaster', () => {
    it('should push to origin master', () => {
      gitUtils.pushToMaster();

      assert(shellStub.exec.calledWith('git push origin master'));
    });
  });
});
