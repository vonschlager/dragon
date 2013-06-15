<apply template="admin">
<div class="well span7 offset2">
<dfForm action="/admin/post/add">
  <fieldset>
    <legend>Dodaj post</legend>
    <dfIfChildErrors>
      <div class="alert alert-error">
        <apply template="button-close"/>
        <dfChildErrorList style="margin-bottom: 0;"/>
      </div>
    </dfIfChildErrors>
    <dfInputText class="span7" ref="title" placeholder="Tytuł"/>
    <dfInputTextArea style="resize: vertical;" class="span7" ref="body" placeholder="Treść" />
    <dfInputSelect ref="kind" />
    <dfInputSubmit class="btn btn-primary" value="Dodaj" />
  </fieldset>
</dfForm>
</div>
</apply>
