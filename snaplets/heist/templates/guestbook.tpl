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
            <a class="accordion-toggle" data-toggle="collapse" data-parent=".sidenav" href="#${year}-months">
              <year/>
            </a>
          </div>
          <div id="${year}-months" class="accordion-body collapse ${in}">
            <div class="accordion-inner">
              <ul class="nav nav-list">
                <months>
                  <a href="/ksiega/${year}/${month}"><monthpretty/></a>
                </months>
              </ul>
            </div>
          </div>
        </div>
      </sidenav>
    </div>
  </div>
  <div class="span9">
    <guestbook>
      <div>
        <h5><nick/></h5>
        <h5><email/></h5>
        <h5><www/></h5>
        <div>
          <body/>
        </div>
        <p class="text-right">
          <small><creation/></small>
        </p>
      </div>
    </guestbook>
  </div>
</div> <!-- row -->
</apply>
