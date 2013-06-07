<!DOCTYPE html>
<html>
  <apply template="head"/>
  <body>
    <div class="container">
      <apply template="nav"/>
      <apply-content/>
    </div>
    <script src="http://code.jquery.com/jquery.js"></script>
    <script src="/static/bootstrap/js/bootstrap.min.js"></script>
    <script src="/static/colorbox/jquery.colorbox-min.js"/>
    <script src="/static/colorbox/jquery.colorbox-pl.js"/>
    <script>$(document).ready(function(){
      $(".cbox").colorbox({rel:'cbox'});
    });
    </script>
  </body>
</html>
