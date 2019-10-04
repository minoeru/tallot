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
  }
}, 500);