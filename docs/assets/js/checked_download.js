function getChcked() {
    let chks = document.querySelectorAll('input[type="checkbox"]');
    let checked = [];
    for (var i = 0; i < chks.length; i++) {
        if (chks[i].checked) {
            checked.push(chks[i].name)
        }
    }
    console.log(checked);
    return checked;
}

$('#button').click(function(){{
    let darr = [];
    let iter = getChcked();
    let name = $('.tabs .active').attr('href').split('#').pop().replace('-', '.') + '.pmat.app'
    for (let i = 0; i < iter.length; i++){{
        console.log(iter.length)
        let url = "https://" + name + '/raw/main/out/figs/' + iter[i] + '.pdf'
        console.log(url)
        darr.push(url)
    }}
    console.log(darr);
    download_files(darr);
    document.querySelectorAll('input[type=checkbox]').forEach(el => el.checked = false);
}})