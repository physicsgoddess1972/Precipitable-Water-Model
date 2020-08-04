describe('getPublishReleaseFunction', () => {
  const chai = require('chai');
  const expect = chai.expect;

  const { parseChangelog } = require('.');

  const CHANGELOG =
`# v22.0.12
## Title of high version

# v12.34.56-beta.156
## Title of a beta release!
Beta description.

# v10.111.2
## Title of version 111.2
### Example
\`\`\`
// notes
\`\`\`

# v0.0.111
## Title of version 1`;

  describe('parseChangelog', () => {
    it('returns correct list of changelog items', () => {
      const items = parseChangelog(CHANGELOG);
      expect(items[0].version).to.equal('22.0.12');
      expect(items[0].releaseTitle).to.equal('Title of high version');
      expect(items[0].prerelease).to.equal(false);

      expect(items[1].version).to.equal('12.34.56-beta.156');
      expect(items[1].releaseTitle).to.equal('Title of a beta release!');
      expect(items[1].releaseDescription).to.equal('Beta description.');
      expect(items[1].prerelease).to.equal(true);

      expect(items[2].version).to.equal('10.111.2');
      expect(items[2].releaseTitle).to.equal('Title of version 111.2');
      expect(items[2].releaseDescription).to.equal(`### Example
\`\`\`
// notes
\`\`\``);

      expect(items[2].prerelease).to.equal(false);

      expect(items[3].version).to.equal('0.0.111');
      expect(items[3].releaseTitle).to.equal('Title of version 1');
      expect(items[3].prerelease).to.equal(false);
    });

    [
      `# 0.0.2
## Title of version 2
`,
      `v0.0.2
## Title of version 2,
`,
      `# v12.34.56-yolo.156
## Title of broken beta release
`,
      `# v12.34.56-beta
## Title of broken beta release
`,
    ].forEach(badlyFormattedChangelog => {
      it('throws when badly formatted CHANGELOG', () => {
        let exception;

        try {
          parseChangelog(badlyFormattedChangelog);
        } catch (e) {
          exception = e;
        }

        expect(exception).to.not.equal(undefined);
      });
    });
  });
});
