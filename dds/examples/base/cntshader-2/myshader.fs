#version 330       
in vec2 fragTexCoord;              
in vec4 fragColor;                 
out vec4 finalColor;               
uniform sampler2D texture0;        
uniform sampler2D alphaTexture;
uniform sampler2D grass;
uniform sampler2D rock;
uniform mat4 cntTransform;
uniform float t;
uniform vec4 colDiffuse;           
void main()                        
{                                  
    vec2 texCoord = ((cntTransform * gl_FragCoord) * 0.4).xy;
    texCoord.x += t * 0.04;
    texCoord.y += t * 0.06;
    vec2 texCoord2 = ((cntTransform * gl_FragCoord) * 2).xy;
    texCoord2.x -= t * 0.05;
    texCoord2.y += t * 0.04;
    vec4 shift = texture2D(grass, texCoord2);
    texCoord.x += (shift.r - 0.5) * 0.2;
    texCoord.y += (shift.g - 0.5) * 0.2;
    finalColor = texture2D(rock, texCoord);

    //finalColor = texture2D(texture0, fragTexCoord);
    /*
    vec4 texelColor = texture(texture0, fragTexCoord);   
    vec4 alphaColor = texture(alphaTexture, fragTexCoord);
    vec4 color = texelColor;
    finalColor = vec4(color.xyz, alpha * color.a);
    */
}
