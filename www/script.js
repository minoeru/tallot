document.addEventListener("DOMContentLoaded", function() {
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
    
    sizuku.addEventListener("animationend", function() {
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
      
      star.addEventListener("animationend", function() {
        this.parentNode.removeChild(this);
      }, false);
    }
  }
}, 500);