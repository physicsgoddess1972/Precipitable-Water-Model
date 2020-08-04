const chai = require('chai');
const sinon = require('sinon');
const proxyquire =  require('proxyquire');

const expect = chai.expect;

describe('utils', () => {
  let utils;

  const ENV_FILE_NAME = '.env';
  const TOKEN_VALUE = 'dsscdsxsaasxsdsd';
  const ENV_FILE_CONTENT = `TOKEN=${TOKEN_VALUE}\n`;
  const VERSION = '0.0.1';
  const REPO_FULLNAME = 'foo/bar';
  const PACKAGE_JSON = {
    version: VERSION,
    repository: {
      url: `https://www.github.com/${REPO_FULLNAME}`,
    },
  };

  const fsStub = {
    readFileSync: sinon.stub(),
  };
  fsStub.readFileSync.withArgs('package.json', 'utf8').returns(JSON.stringify(PACKAGE_JSON));
  fsStub.readFileSync.withArgs('.env', 'utf8').returns(ENV_FILE_CONTENT);

  before(() => {
    utils = proxyquire('.', {
      fs: fsStub,
    });
  });

  describe('extractEnv', () => {
    it('should read from provided file', () => {
      const env = utils.extractEnv(ENV_FILE_NAME);

      sinon.assert.calledWith(fsStub.readFileSync, ENV_FILE_NAME, 'utf8');
    });

    it('should return correct key value map', () => {
      fsStub.readFileSync.withArgs(ENV_FILE_NAME, 'utf8').returns(ENV_FILE_CONTENT);
      const env = utils.extractEnv(ENV_FILE_NAME);

      expect(env.TOKEN).to.equal(TOKEN_VALUE);
    });
  });

  describe('getRepoFullnameFromPackage', () => {
    it('should return correct value from package.json', () => {
      const repoName = utils.getRepoFullnameFromPackage();

      expect(repoName).to.equal(REPO_FULLNAME);
    });
  });

  describe('getVersionFromPackage', () => {
    it('should return correct value from package.json', () => {
      const repoName = utils.getVersionFromPackage();

      expect(repoName).to.equal(VERSION);
    });
  });
});
