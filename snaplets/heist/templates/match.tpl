<apply template="index">
<script>
$(function(){
  $('.sidenav').affix();
});
</script>
<div class="row">
  <div class="span3 sidebar">
    <div class="accordion sidenav">
      <sidenav>
        <div class="accordion-group">
          <div class="accordion-heading">
            <a class="accordion-toggle" data-toggle="collapse" data-parent=".sidenav" href="#${nr}-match">
              <nr/> Rajd Harcerski Zapałka '18
            </a>
          </div>
          <div id="${year}-zapalka" class="accordion-body collapse ${in}">
            <div class="accordion-inner">
              <ul class="nav nav-list">
                <contents>
                  <li class="${active}">
                    <a href="/zapalka/${nr}">Rajd</a>
                  </li>
                </contents>
              </ul>
            </div>
          </div>
        </div>
      </sidenav>
    </div>
  </div>
  <div class="span9">
    <news>
      <div>
        <h3><title/></h3>
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
