function parseChangelogItem(item) {
  const regex = /#\s?(v\d+\.\d+\.?\d*(-(beta|alpha|rc)\.\d+)?)\n+##\s?(.*)([\s\S]*)/g;
  const match = regex.exec(item);

  const { 1: tagName, 2: prereleasePart, 4: releaseTitle, 5: description } = match;
  const releaseDescription = description.trim();
  const prerelease = !!prereleasePart;

  if (!tagName || !releaseTitle) throw new Error(badlyFormattedChangelog(`tagName=${tagName},releaseTitle=${releaseTitle}`));

  const version = tagName.replace('v', '');

  return {
    version,
    releaseTitle,
    releaseDescription,
    prerelease,
  };
}

const badlyFormattedChangelog = (errorDetails) => `Your CHANGELOG.md seems to be badly formatted.
Every item should start with:
# v1.0.0
## Release title

Error details:
${errorDetails}`;

function parseChangelog(stdOut) {
  try {
    return getItemsAsStrings(stdOut).map(parseChangelogItem);
  } catch (e) {
    throw new Error(badlyFormattedChangelog(e));
  }
}

function getItemsAsStrings(changelog) {
  const items = [];

  const regexMatches = getRegexMatchesForChangelogItems(changelog);

  if (regexMatches.length < 1) throw new Error(badlyFormattedChangelog('Unable to find regex matches'));

  for (let i = 0; i < regexMatches.length; i++) {
    const match = regexMatches[i];
    if (i < regexMatches.length - 1) {
      const nextMatch = regexMatches[i + 1];
      items.push(changelog.substring(match.index, nextMatch.index));
    } else {
      items.push(changelog.substring(match.index));
    }
  }

  return items;
}

function getRegexMatchesForChangelogItems(changelog) {
  const itemVersionRegex = /#\s?(v\d+\.\d+\.?\d*)/g;
  let match;
  const regexMatches = [];
  while ((match = itemVersionRegex.exec(changelog)) !== null) {
    regexMatches.push(match);
  }
  return regexMatches;
}

module.exports = {
  parseChangelog,
  parseChangelogItem,
};
