/**
 * Created by yannickgrenzinger on 25/05/2016.
 */
var build = function(name) {
    var self = this;
    var _name = name;

    var _display = function() {
        console.log(self._name);
    };

    return {
        name: _name,
        display: _display
    }
};