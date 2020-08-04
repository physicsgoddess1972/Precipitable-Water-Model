module.exports = {
  extends: '@transferwise',
  rules: {
    'arrow-parens': ['error', 'as-needed'],
    'class-methods-use-this': ['warn'],
    'import/prefer-default-export': 1,
    'import/no-named-as-default': 1,
    'max-len': ['warn', 120],
    'no-param-reassign': [2, { props: false }],
    'no-restricted-syntax': ['off', 'FunctionExpression', 'WithStatement'],
    'no-use-before-define': ['error', { functions: false, classes: true }],
    'one-var': ['error', { const: 'never', let: 'always' }],
    'one-var-declaration-per-line': ['error', 'initializations'],
    'space-before-function-paren': ['error', 'never'],
  },
  globals: {
    window: false,
    describe: false,
    it: false,
    beforeEach: false,
    inject: false,
    expect: false,
    angular: false,
    sinon: false,
    document: false,
  },
  settings: {
    'import/resolver': {
      webpack: {
        config: 'webpack/webpack.config.js',
      },
    },
  },
};
