const chai = require('chai');

const expect = chai.expect;
const sinon = require('sinon');
const sinonChai = require('sinon-chai');
const proxyquire = require('proxyquire');

chai.use(sinonChai);

describe('publish-release-with-changelog', () => {
  const EXIT_1_ERROR = new Error('exit 1');
  const CHANGELOG_FILE_NAME = 'CHANGELOG.md';
  const CHANGELOG_FILE_CONTENT = 'default changelog stdout';
  const VERSION = '0.0.1';
  const REPO_FULLNAME = 'foo/bar';
  const GITHUB_TOKEN = 'github-token-123';

  const getVersionFromPackageMock = sinon.stub();
  const getRepoFullnameFromPackageMock = sinon.stub();
  const getPublishReleaseFunctionMock = sinon.stub();
  const publishReleaseMock = sinon.stub();
  publishReleaseMock.returns(Promise.resolve({}));
  getPublishReleaseFunctionMock.returns(publishReleaseMock);
  const parseChangelogMock = sinon.stub();

  const shellStub = {
    exit: sinon.stub(),
  };
  shellStub.exit.withArgs(1).throws(EXIT_1_ERROR);

  const fsStub = {
    readFileSync: sinon.stub(),
  };
  fsStub.readFileSync.withArgs(CHANGELOG_FILE_NAME, 'utf8').throws(CHANGELOG_FILE_CONTENT);


  function aChangeLogItem(version = VERSION, releaseTitle = 'Release title', releaseDescription, prerelease) {
    return { version, releaseTitle, releaseDescription, prerelease };
  }

  beforeEach(() => {
    process.env.GITHUB_TOKEN = GITHUB_TOKEN;
    getRepoFullnameFromPackageMock.returns(REPO_FULLNAME);
    getVersionFromPackageMock.returns(VERSION);
  });

  afterEach(() => {
    shellStub.exit.reset();
    publishReleaseMock.reset();
  });

  it('should exit if no GITHUB_TOKEN in env', () => {
    delete process.env.GITHUB_TOKEN;
    try {
      const publishReleaseWithChangelog = requirePublishReleaseWithChangelog();
      publishReleaseWithChangelog();
    } catch (e) {
      expect(shellStub.exit).to.have.been.calledWith(1);
    }
  });

  it('should exit if no repo fullname', () => {
    getRepoFullnameFromPackageMock.returns(undefined);
    try {
      const publishReleaseWithChangelog = requirePublishReleaseWithChangelog();
      publishReleaseWithChangelog();
    } catch (e) {
      expect(shellStub.exit).to.have.been.calledWith(1);
    }
  });

  it('should exit if bad repo fullname', () => {
    getRepoFullnameFromPackageMock.returns('eeee');
    try {
      const publishReleaseWithChangelog = requirePublishReleaseWithChangelog();
      publishReleaseWithChangelog();
    } catch (e) {
      expect(shellStub.exit).to.have.been.calledWith(1);
    }
  });

  it('should pass the CHANGELOG.md file to the parser', () => {
    parseChangelogMock.returns([aChangeLogItem(VERSION)]);
    fsStub.readFileSync.withArgs(CHANGELOG_FILE_NAME, 'utf8').returns('lala');
    const publishReleaseWithChangelog = requirePublishReleaseWithChangelog();
    publishReleaseWithChangelog();

    const fsCall = fsStub.readFileSync.getCall(0);
    expect(fsCall.args[0]).to.equal(CHANGELOG_FILE_NAME);
    const parserCall = parseChangelogMock.getCall(0);
    expect(parserCall.args[0]).to.equal('lala');
  });

  it('should exit if versions from package and last changelog item differ', () => {
    parseChangelogMock.returns([aChangeLogItem(VERSION)]);
    fsStub.readFileSync.returns(CHANGELOG_FILE_CONTENT);
    getVersionFromPackageMock.returns('0.0.2');
    parseChangelogMock.returns([aChangeLogItem('0.0.3')]);

    try {
      const publishReleaseWithChangelog = requirePublishReleaseWithChangelog();
      publishReleaseWithChangelog();
    } catch (e) {
      expect(shellStub.exit).to.have.been.calledWith(1);
    }
  });

  it('should publish release with tag name as version prefixed by "v" with title', () => {
    parseChangelogMock.returns([aChangeLogItem(VERSION)]);
    fsStub.readFileSync.returns(CHANGELOG_FILE_CONTENT);
    getVersionFromPackageMock.returns(VERSION);
    parseChangelogMock.returns([
      aChangeLogItem(VERSION, 'title'),
      aChangeLogItem('0.0.3'),
    ]);

    const publishReleaseWithChangelog = requirePublishReleaseWithChangelog();
    publishReleaseWithChangelog();

    expect(publishReleaseMock).to.have.been.calledWith(`v${VERSION}`, 'title');
  });

  it('should publish release with title and description if present in last changelog item', () => {
    getVersionFromPackageMock.returns(VERSION);
    fsStub.readFileSync.returns(CHANGELOG_FILE_CONTENT);
    parseChangelogMock.returns([
      aChangeLogItem(VERSION, 'title', 'description lala'),
      aChangeLogItem('0.0.3'),
    ]);

    const publishReleaseWithChangelog = requirePublishReleaseWithChangelog();
    publishReleaseWithChangelog();

    expect(publishReleaseMock)
      .to.have.been.calledWith(`v${VERSION}`, 'title', 'description lala');
  });

  it('should publish a pre-release if the last changelog item is a pre-release', () => {
    getVersionFromPackageMock.returns(VERSION);
    fsStub.readFileSync.returns(CHANGELOG_FILE_CONTENT);
    parseChangelogMock.returns([
      aChangeLogItem(VERSION, 'title', 'description lala', true),
      aChangeLogItem('0.0.3'),
    ]);

    const publishReleaseWithChangelog = requirePublishReleaseWithChangelog();
    publishReleaseWithChangelog();

    expect(publishReleaseMock)
      .to.have.been.calledWith(`v${VERSION}`, 'title', 'description lala', true);
  });

  it('should exit with 1 if publish fails', () => {
    getVersionFromPackageMock.returns(VERSION);
    fsStub.readFileSync.returns(CHANGELOG_FILE_CONTENT);
    parseChangelogMock.returns([
      aChangeLogItem(VERSION, 'title', 'description lala'),
      aChangeLogItem('0.0.3'),
    ]);
    publishReleaseMock.returns(Promise.reject('errorrrrr'));

    const publishReleaseWithChangelog = requirePublishReleaseWithChangelog();
    return publishReleaseWithChangelog().catch(err => {
      expect(err).to.equal(EXIT_1_ERROR);
      expect(shellStub.exit).to.have.been.calledWith(1);
    });
  });

  function requirePublishReleaseWithChangelog() {
    return proxyquire('.', {
      shelljs: shellStub,
      fs: fsStub,
      '../utils': {
        getVersionFromPackage: getVersionFromPackageMock,
        getRepoFullnameFromPackage: getRepoFullnameFromPackageMock,
      },
      '../publish-github-release': {
        getPublishReleaseFunction: getPublishReleaseFunctionMock,
      },
      '../parser': {
        parseChangelog: parseChangelogMock,
      },
    });
  }
});
