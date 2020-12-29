// Include gh.js
const GitHub = require("gh.js");

// Create the GitHub instance
var gh = new GitHub();

// Get my public events
gh.get("users/physicsgoddess1972/Precipitable-Water-Model/events", (err, res) => {
    if (err) { return console.error(err); }

    // Filter by PushEvent type
    var pushEvents = res.filter(c => {
        return c.type === "PushEvent";
    });

    // Show the date and the repo name
    console.log(pushEvents.map(c => {
        return "Pushed at " + c.created_at + " in " + c.repo.name;
    }).join("\n"));
    // => Pushed at 2015-11-17T11:05:04Z in jillix/jQuery-json-editor
    // => Pushed at 2015-11-16T18:56:05Z in IonicaBizau/html-css-examples
    // => Pushed at 2015-11-16T16:36:37Z in jillix/node-cb-buffer
    // => Pushed at 2015-11-16T16:35:57Z in jillix/node-cb-buffer
    // => Pushed at 2015-11-16T16:34:58Z in jillix/node-cb-buffer
    // => Pushed at 2015-11-16T13:39:33Z in IonicaBizau/ghosty
});
