#version 330       
in vec2 fragTexCoord;              
in vec4 fragColor;                 
out vec4 finalColor;               
uniform sampler2D texture0;        
uniform vec4 colDiffuse;           
uniform mat4 cntTransform;
void main()                        
{                                  
    vec4 texelColor = texture(texture0 , fragTexCoord);   
    vec4 cntCoord =  cntTransform * gl_FragCoord;
    float r = smoothstep(0, 50, cntCoord.x);
    float g = smoothstep(0, 50, cntCoord.y);
    if (cntCoord.x > 50) discard;
    if (cntCoord.x < 0) discard;
    if (cntCoord.y > 50) discard;
    if (cntCoord.y < 0) discard;
    finalColor = vec4(r, g, 0, 1);
}
