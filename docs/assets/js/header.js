(function () {
    $.get("assets/external/head.html", ).done(function (data) {
        data = data.replace(/file_name/g, fname);
        data = data.replace(/branch/g, bname);
        $("#content").html(data);
    });
})();