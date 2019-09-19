#version 330                       

in vec3 vertexPosition;            
in vec2 vertexTexCoord;            
in vec4 vertexColor;               
out vec2 fragTexCoord;             
out vec4 fragColor;                
uniform mat4 mvp;                  

//uniform mat4 cntTransform;
uniform float minheight;
uniform float maxheight;

void main()                        
{                                  
    fragTexCoord = vertexTexCoord; 
    gl_Position = mvp*vec4(vertexPosition, 1);
    float alpha = smoothstep(minheight, maxheight, vertexPosition.y);
    fragColor = vec4(vertexColor.rgb, alpha);
} 
