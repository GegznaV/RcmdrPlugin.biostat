var sortableListBinding = new Shiny.InputBinding();
$.extend(sortableListBinding, {
  find: function(scope) {
    return $(scope).find(".sortableList");
  },
  getValue: function(el) {
    if (typeof Sortable.active != 'undefined'){
      return Sortable.active.toArray();
    }
    else return "";
  },
  subscribe: function(el, callback) {
    $(el).on("change.sortableListBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".sortableListBinding");
  },
  initialize: function(el) {
    Sortable.create(el,{
      onUpdate: function (evt) {
          $(el).trigger("change");
    }});
  }
});

Shiny.inputBindings.register(sortableListBinding);