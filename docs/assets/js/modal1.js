
$( document ).ready(function() {
    if (document.cookie.indexOf('visited=true') == -1){
        // load the overlay
        $('#modal1').modal('open');

       var year = 1000*60*60*24*365;
        var expires = new Date((new Date()).valueOf() + year);
        document.cookie = "visited=true;expires=" + expires.toUTCString();

    }
});