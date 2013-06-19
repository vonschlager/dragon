<apply template="index">
<script type="text/javascript">
  $(function(){
    $('a.thumbnail').tooltip();
  });
</script>
<ul class="thumbnails">
<albums>
  <li class="span2">
    <a href="/zdjecia/${albumid}" class="thumbnail" title="${title}">
      <img class="lazy" data-original="${thumb}" src="/static/white.gif" alt="" width="130" height="130" />
    </a>
  </li>
</albums>
</ul>
</apply>
