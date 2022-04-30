$(function(){
  $("#after").datepicker({
    altField: "#afters",
    altFormat: "@",
    changeMonth: true,
    changeYear: true,
    defaultDate: "-7d",
    minDate: "01/01/1970",
    yearRange: "2000:+0",
    onSelect: function(selectedDate) {
      $("#before").datepicker("option","minDate",selectedDate);
    }
  });
  $("#before").datepicker({
    altField: "#befores",
    altFormat: "@",
    changeMonth: true,
    changeYear: true,
    defaultDate: "+1d",
    maxDate: "+1d",
    yearRange: "2000:+0",
    onSelect: function(selectedDate) {
      $("#after").datepicker("option","maxDate",selectedDate);
    }
  });
  $(function(){
  $('.multipart-alternative').tabs()
  });
  $(function(){
      $('.embedded-html').on('load',function(){
      this.style.height = this.contentWindow.document.body.offsetHeight + 'px';
    });
  });
});

