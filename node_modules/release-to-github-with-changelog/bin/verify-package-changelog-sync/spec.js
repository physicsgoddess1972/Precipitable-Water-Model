describe('verify-package-changelog-sync', () => {
  const chai = require('chai');
  const expect = chai.expect;
  const sinon = require('sinon');
  const sinonChai = require("sinon-chai");
  chai.use(sinonChai);

  const proxyquire =  require('proxyquire');

  const EXIT_1_ERROR = new Error('exit 1');

  const getVersionFromPackageMock = sinon.stub();
  const getPublishReleaseFunctionMock = sinon.stub();
  const parseChangelogMock = sinon.stub();

  const shellStub = {
    exit: sinon.stub(),
    cat: sinon.stub(),
  };

  shellStub.exit.withArgs(1).throws(EXIT_1_ERROR);

  const CHANGELOG_FILE_NAME = 'CHANGELOG.md';
  const CHANGELOG_FILE_OUT = 'default changelog stdout';
  const VERSION = '0.0.1';

  function aChangeLogItem(version = VERSION, releaseTitle = 'Release title', releaseDescription) {
    return { version, releaseTitle, releaseDescription };
  }

  beforeEach(() => {
    getVersionFromPackageMock.returns(VERSION);
  });

  afterEach(() => {
    shellStub.cat.reset();
    shellStub.exit.reset();
  });

  it('should cat the CHANGELOG.md file to the parser', () => {
    parseChangelogMock.returns([aChangeLogItem(VERSION)]);
    shellStub.cat.withArgs(CHANGELOG_FILE_NAME).returns('lala');
    const publishReleaseWithChangelog = requireVerifyPackageChangelogSync();
    publishReleaseWithChangelog();

    const catCall = shellStub.cat.getCall(0);
    expect(catCall.args[0]).to.equal('CHANGELOG.md');
    const parserCall = parseChangelogMock.getCall(0);
    expect(parserCall.args[0]).to.equal('lala');
  });

  it('should exit if exception from parsing', () => {
    const error = new Error('Parsing error');
    parseChangelogMock.throws(error);
    shellStub.cat.withArgs(CHANGELOG_FILE_NAME).returns('lala');
    const publishReleaseWithChangelog = requireVerifyPackageChangelogSync();

    try {
      publishReleaseWithChangelog();
    } catch (e) {
      expect(shellStub.exit).to.have.been.calledWith(1);
    }
  });

  it('should exit if versions from package and last changelog item differ', () => {
    parseChangelogMock.returns([aChangeLogItem(VERSION)]);
    shellStub.cat.returns(CHANGELOG_FILE_OUT);
    getVersionFromPackageMock.returns('0.0.2');
    parseChangelogMock.returns([aChangeLogItem('0.0.3')]);

    try {
      const publishReleaseWithChangelog = requireVerifyPackageChangelogSync();
      publishReleaseWithChangelog();
    } catch (e) {
      expect(shellStub.exit).to.have.been.calledWith(1);
    }
  });

  function requireVerifyPackageChangelogSync() {
    return proxyquire('.', {
      shelljs: shellStub,
      '../utils': {
        getVersionFromPackage: getVersionFromPackageMock,
      },
      '../parser': {
        parseChangelog: parseChangelogMock,
      },
    });
  }
});
