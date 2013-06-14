<apply template="index">
<dfForm action="/login">
  <fieldset>
    <legend>Logowanie</legend>
    <br/>
    <dfLabel ref="login" />
    <dfInputText ref="login" />
    <dfChildErrorList ref="login" />
    <br/>
    <dfLabel ref="password" />
    <dfInputPassword ref="password" />
    <dfChildErrorList ref="password" />
    <br/>
    <dfInputSubmit class="btn" value="Submit" />
  </fieldset>
</dfForm>
</apply>
