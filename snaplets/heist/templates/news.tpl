<apply template="index">
<div class="row">
  <div class="span3 sidebar">
    <div class="accordion sidenav">
      <sidenav>
        <div class="accordion-group">
          <div class="accordion-heading text-centre">
            <a class="accordion-toggle" data-toggle="collapse" data-parent=".sidenav" href="#${year}-months">
              <year/>
            </a>
          </div>
          <div id="${year}-months" class="accordion-body collapse ${in}">
            <div class="accordion-inner">
              <ul class="nav nav-list">
                <months>
                  <a href="/wiesci/${year}/${month}"><monthpretty/></a>
                </months>
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
