var myObj = {}

myObj.anc = function(){
    $.get("assets/external/side_nav.html", function(data){
        if (document.location.pathname == '/' ){
            var path = "/index.html";
        } else{
            if (document.location.pathname.includes(".html")){
                var path = document.location.pathname;   
            } else{
                var path = document.location.pathname + ".html"
            }
        }
        var page1 = path.split("/").pop().replace(".", "-");
        document.getElementById(page1).parentElement.setAttribute('class', 'submenu show');
        var page2 = document.getElementById(page1).classList;
        page2.add("mdl-navigation__link--current");
        page2.remove("mdl-navigation__link");
    })
};

myObj.gnc = function(){

    document.querySelectorAll('.sidebar .nav-link').forEach(function(element) {

        element.addEventListener('click', function(e) {

            let nextEl = element.nextElementSibling;
            let parentEl = element.parentElement;
            console.log(nextEl)
            console.log(parentEl)
            if (nextEl) {
                let mycollapse = new bootstrap.Collapse(nextEl);

                if (nextEl.classList.contains('show')) {
                    mycollapse.hide();
                } else {
                    mycollapse.show();
                    // find other submenus with class=show
                }
                var opened_submenu = parentEl.parentElement.querySelector('.submenu.show');
                // if it exists, then close all of them
                if (opened_submenu) {
                    new bootstrap.Collapse(opened_submenu);
                }
            }

        });
    })

};
myObj.anc();
myObj.gnc();