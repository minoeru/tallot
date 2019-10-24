window.onload = function () {
    //click effect
    document.body.addEventListener("click", drop, false);
    let title_scene_child = document.getElementById('title_ui');
    let title_scene = title_scene_child.parentNode;

    function drop(e) {
        let x = e.pageX;
        let y = e.pageY;

        let sizuku = document.createElement("div");
        sizuku.style.top = y + "px";
        sizuku.style.left = x + "px";
        title_scene.appendChild(sizuku);
        sizuku.className = "sizuku";

        sizuku.addEventListener("animationend", function () {
            this.parentNode.removeChild(this);
        }, false);

        for (var i = 1; i <= 3; i++) {
            let star = document.createElement("div");
            let star_size = Math.random() * 50 + 30;
            star.style.width = star_size + "px";
            star.style.height = star_size + "px";
            star.style.top = y - Math.random() * 20 - star_size / 2 + "px";
            star.style.left = x + Math.random() * 150 - 75 - star_size / 2 + "px";
            star.style.animation = " star " + Math.random() * 3 + 2 + "s ease " + Math.random() * 0.3 + "s";
            star.style.transform = "rotate(" + Math.random() * 360 + "deg)";
            title_scene.appendChild(star);
            star.className = "star";

            star.addEventListener("animationend", function () {
                this.parentNode.removeChild(this);
            }, false);
        }
    }
};

function shuffle() {
    for (let j = 1; j < 14; j++) {
        let id_num = "mob" + j;
        let add_class_num = id_num + "_move";
        document.getElementById(id_num).classList.add(add_class_num);
        document.getElementById(id_num).addEventListener("animationend", function () {
            document.getElementById(id_num).classList.remove(add_class_num);
        }, false);
    }
    document.getElementById('mainCard').classList.add('mainCard_move');
    document.getElementById('mainCard').addEventListener("animationend", function () {
        document.getElementById('mainCard').classList.remove('mainCard_move');
    }, false);
    document.getElementById('cardimage').classList.add('fade_in');
}

function cartain(e) {
    const cartain_container = document.createElement("div");
    document.body.appendChild(cartain_container);
    cartain_container.classList.add("cartain_container");
    setTimeout(function () {
        cartain_container.parentNode.removeChild(cartain_container);
    }, 1700);

    for (let i = 0; i < 14; i++) {
        let cartain = document.createElement("div");
        cartain.style.width = 100 / 7 + "vw";
        cartain.style.height = 100 + "vh";
        if (i < 7) {
            cartain.style.top = 100 + "vh";
        } else {
            cartain.style.top = -100 + "vh";
        }

        cartain.style.left = 100 / 7 * (i % 7) + "vw";
        cartain.style.position = "absolute";
        cartain.style.background = "#433c4a";
        cartain.style.opacity = "0";
        cartain.style.animation = "cartain-fall .5s ease " + Math.abs(6 - i) * 0.1 + "s 2 alternate forwards";
        cartain_container.appendChild(cartain);
    }
    
   let title_pane = document.getElementById('title_ui').parentNode;
   let result_pane = document.getElementById('Back_to_start_button_ui').parentNode;
   let gallery_pane = document.getElementById('Back_to_start_button_ui2').parentNode;
   let credit_pane = document.getElementById('Back_to_start_button_ui3').parentNode;
   let shuffle_pane = document.getElementById('put_button_ui').parentNode;
   title_pane.style.opacity = 0;
   result_pane.style.opacity = 0;
   gallery_pane.style.visibility = "hidden";
   credit_pane.style.opacity = 0;
   shuffle_pane.style.opacity = 0;
   title_pane.style.animation = "fadeIn .5s ease .5s forwards";
   result_pane.style.animation = "fadeIn .5s ease .5s forwards";
   gallery_pane.style.animation = "fadeIn_2 .5s ease .5s forwards";
   credit_pane.style.animation = "fadeIn .5s ease .5s forwards";
   shuffle_pane.style.animation = "fadeIn .5s ease .5s forwards";
}
