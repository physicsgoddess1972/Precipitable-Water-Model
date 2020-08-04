describe('getPublishReleaseFunction', () => {
  const chai = require('chai');
  const expect = chai.expect;
  const nock = require('nock');
  const GITHUB_REPOS_URI = 'https://api.github.com/repos';

  const REPO_FULLNAME = 'foo/bar';
  const TOKEN = 'token';
  const VERSION = '0.0.1';
  const RELEASE_TITLE = 'This release is awesome';

  // when no overrides are specified, path.extname behaves normally
  const { getPublishReleaseFunction } = require('.');

  describe('publishRelease default', () => {
    let publishRelease;
    beforeEach(() => {
      publishRelease = getPublishReleaseFunction(REPO_FULLNAME, TOKEN);
    });

    it('should exit if no version provided', () => (
      publishRelease().catch(err => {
        expect(err).to.contain('version');
      })
    ));

    it('should exit if no title provided', () => (
      publishRelease('0.0.1').catch(err => {
        expect(err).to.contain('title');
      })
    ));

    it('should make correct http call and use master by default', () => {
      const expectedReleaseResourceCmd = {
        tag_name: `v${VERSION}`,
        target_commitish: 'master',
        name: RELEASE_TITLE,
      };

      const expectedHeaders = {
        Accept: 'application/vnd.github.v3+json',
        'User-Agent': REPO_FULLNAME.split('/')[0],
        Authorization: `token ${TOKEN}`,
        'Content-Type': 'application/json',
      };

      const httpExpect = nock(GITHUB_REPOS_URI, { reqheaders: expectedHeaders })
        .post(`/${REPO_FULLNAME}/releases`, expectedReleaseResourceCmd)
        .reply(200, {
          id: '123ABC',
        });

      return publishRelease(VERSION, RELEASE_TITLE)
        .then(() => {
          expect(httpExpect.isDone()).to.equal(true);
        });
    });

    it('should add release description to release resource if provided', () => {
      const releaseDescription = 'Wonderful description';
      const expectedReleaseResourceCmd = {
        tag_name: `v${VERSION}`,
        target_commitish: 'master',
        name: RELEASE_TITLE,
        body: releaseDescription,
      };

      const httpExpect = nock(GITHUB_REPOS_URI)
        .post(`/${REPO_FULLNAME}/releases`, expectedReleaseResourceCmd)
        .reply(200, {
          id: '123ABC',
        });

      return publishRelease(VERSION, RELEASE_TITLE, releaseDescription)
        .then(() => {
          expect(httpExpect.isDone()).to.equal(true);
        });
    });

    it('should run a pre-release instead if prerelease provided', () => {
      const releaseDescription = 'Wonderful description';
      const expectedReleaseResourceCmd = {
        tag_name: `v${VERSION}`,
        target_commitish: 'master',
        name: RELEASE_TITLE,
        body: releaseDescription,
        prerelease: true,
      };

      const httpExpect = nock(GITHUB_REPOS_URI)
        .post(`/${REPO_FULLNAME}/releases`, expectedReleaseResourceCmd)
        .reply(200, {
          id: '123ABC',
        });

      return publishRelease(VERSION, RELEASE_TITLE, releaseDescription, true)
        .then(() => {
          expect(httpExpect.isDone()).to.equal(true);
        });
    });
  });

  describe('publishRelease for branch', () => {
    it('should use specified branch', () => {
      const publishRelease = getPublishReleaseFunction(REPO_FULLNAME, TOKEN, 'release');

      const httpExpect = nock(GITHUB_REPOS_URI)
        .post(`/${REPO_FULLNAME}/releases`, body => body.target_commitish === 'release')
        .reply(200, {
          id: '123ABC',
        });

      return publishRelease(VERSION, RELEASE_TITLE)
        .then(() => expect(httpExpect.isDone()).to.equal(true));
    });
  });
});
