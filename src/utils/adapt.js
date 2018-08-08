
/*rem处理*/
export default function () {
  let designWidth = 750
  let  rem2px = 100
  let d = window.document.createElement('div');
  d.style.width = '1rem';
  d.style.display = "none";
  let head = window.document.getElementsByTagName('head')[0];
  head.appendChild(d);
  let defaultFontSize = parseFloat(window.getComputedStyle(d, null).getPropertyValue('width'));
  d.remove();
  document.documentElement.style.fontSize = window.innerWidth / designWidth * rem2px / defaultFontSize * 100 + '%';
  let st = document.createElement('style');
  let portrait = "@media screen and (min-width: "+window.innerWidth+"px) {html{font-size:"+ ((window.innerWidth/(designWidth/rem2px)/defaultFontSize)*100) +"%;}}";
  let landscape = "@media screen and (min-width: "+window.innerHeight+"px) {html{font-size:"+ ((window.innerHeight/(designWidth/rem2px)/defaultFontSize)*100) +"%;}}"
  st.innerHTML = portrait + landscape;
  head.appendChild(st);
  return defaultFontSize
}
