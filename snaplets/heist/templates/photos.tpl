<apply template="index">
<script src="/static/jquery.lazyload.min.js"/>
<script>
$(function(){
  $("img.lazy").lazyload({effect:"fadeIn"});
});
</script>
<script src="/static/colorbox/jquery.colorbox-min.js"/>
<script>
$(document).ready(function(){
  $(".cbox").colorbox({
    next:"następne",
    previous:"poprzednie",
    current:"{current} z {total}",
    close:"",
    maxHeight:"700",
    rel:"cbox"
  });
});
</script>
<ul class="thumbnails">
<photos>
  <li class="span2">
    <a href="${url}" class="thumbnail cbox">
      <img class="lazy" data-original="${thumb}" src="/static/whitedot.gif" alt="" width="130" height="130" />
    </a>
  </li>
</photos>
</ul>
</apply>
