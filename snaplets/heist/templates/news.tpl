<apply template="index">
<script>
$(function(){
  $('.sidenav').affix();
});
</script>
<div class="row">
  <div class="span3 sidebar">
    <ul class="nav nav-tabs nav-stacked sidenav">
      <sidenav>
        <li><a href="#"><year/></a>
        <months>
          <ul>
            <li><a href="#"><month/></a></li>
          </ul>
        </months>
        </li>
      </sidenav>
    </ul>
  </div>
  <div class="span9">
    <news>
      <div>
        <h3><a href="/wpis/pokaz/${id}"><title/></a></h3>
        <div>
          <body/>
        </div>
        <p class="text-right">
          <small><publish/></small>
        </p>
      </div>
    </news>
  </div>
</div> <!-- row -->
</apply>
