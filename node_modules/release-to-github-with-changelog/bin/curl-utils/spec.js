describe('curl-utils', () => {
  const curlUtils = require('.');
  const expect = require('chai').expect;

  describe('formCurlHeader', () => {
    it('should return correct curl string for header', () => {
      const header = curlUtils.formCurlHeader('foo', 'bar');

      expect(header).to.equal('-H "foo: bar"');
    });
  });
});
