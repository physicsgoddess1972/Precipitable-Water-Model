#! /usr/bin/env node

function formCurlHeader(name, value) {
  return `-H "${name}: ${value}"`;
}

module.exports = {
  formCurlHeader,
};
