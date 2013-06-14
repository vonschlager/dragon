<!DOCTYPE html>
<html>
  <apply template="head"/>
  <body>
    <div class="container">
      <div class="well span5 offset3">
        <dfForm action="/admin">
          <fieldset>
            <legend>Logowanie</legend>
              <div class="alert alert-error">
                <apply template="button-close"/>
                <ul style="margin-bottom: 0;"><li>Nieprawidłowy użytkownik lub hasło</li></ul>
              </div>
            <dfLabel ref="login" />
            <dfInputText class="span5" ref="login" placeholder="Login" />
            <dfLabel ref="password" />
            <dfInputPassword class="span5" ref="password" placeholder="Hasło" />
            <dfInputSubmit class="btn btn-primary" value="Zaloguj" />
          </fieldset>
        </dfForm>
      </div>
    </div>
  </body>
</html>
