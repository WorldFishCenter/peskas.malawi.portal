$( document ).ready(function() {
  // Define tooltip functions in global scope
  window.hexagonTooltip = function(object) {
    if (!object) return null;
    const totalActivities = object.points.length;
    return `
      <div class='tooltip-content'>
        <div class='tooltip-header'>Activity Cluster</div>
        <div class='tooltip-body'>
          <p><strong>Location:</strong> ${object.position[1].toFixed(3)}°N, ${object.position[0].toFixed(3)}°E</p>
          <p><strong>Activities:</strong> ${totalActivities}</p>
        </div>
      </div>
    `;
  };

  window.pathTooltip = function(object) {
    if (!object) return null;
    return `
      <div class='tooltip-content'>
        <div class='tooltip-header'>Fishing Trip Details</div>
        <div class='tooltip-body'>
          <p><strong>Catch:</strong> ${object.catch_kg.toFixed(1)} kg</p>
          <p><strong>Species:</strong> ${object.catch_taxon}</p>
          <p><strong>Duration:</strong> ${object.trip_duration.toFixed(1)} hours</p>
          <p><strong>Distance:</strong> ${object.total_distance.toFixed(2)} units</p>
        </div>
      </div>
    `;
  };
});
