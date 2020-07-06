<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis labelsEnabled="0" readOnly="0" simplifyDrawingTol="1" simplifyLocal="1" styleCategories="AllStyleCategories" version="3.8.3-Zanzibar" simplifyDrawingHints="0" simplifyAlgorithm="0" maxScale="0" hasScaleBasedVisibilityFlag="0" minScale="1e+08" simplifyMaxScale="1">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
  </flags>
  <renderer-v2 symbollevels="0" enableorderby="0" type="singleSymbol" forceraster="0">
    <symbols>
      <symbol clip_to_extent="1" force_rhr="0" type="marker" alpha="1" name="0">
        <layer class="SimpleMarker" pass="0" locked="0" enabled="1">
          <prop k="angle" v="0"/>
          <prop k="color" v="102,102,102,255"/>
          <prop k="horizontal_anchor_point" v="1"/>
          <prop k="joinstyle" v="bevel"/>
          <prop k="name" v="circle"/>
          <prop k="offset" v="0,0"/>
          <prop k="offset_map_unit_scale" v="3x:0,0,0,0,0,0"/>
          <prop k="offset_unit" v="MM"/>
          <prop k="outline_color" v="35,35,35,255"/>
          <prop k="outline_style" v="no"/>
          <prop k="outline_width" v="0"/>
          <prop k="outline_width_map_unit_scale" v="3x:0,0,0,0,0,0"/>
          <prop k="outline_width_unit" v="MM"/>
          <prop k="scale_method" v="diameter"/>
          <prop k="size" v="0.2"/>
          <prop k="size_map_unit_scale" v="3x:0,0,0,0,0,0"/>
          <prop k="size_unit" v="MM"/>
          <prop k="vertical_anchor_point" v="1"/>
          <data_defined_properties>
            <Option type="Map">
              <Option value="" type="QString" name="name"/>
              <Option name="properties"/>
              <Option value="collection" type="QString" name="type"/>
            </Option>
          </data_defined_properties>
        </layer>
      </symbol>
    </symbols>
    <rotation/>
    <sizescale/>
  </renderer-v2>
  <customproperties>
    <property value="0" key="embeddedWidgets/count"/>
    <property key="variableNames"/>
    <property key="variableValues"/>
  </customproperties>
  <blendMode>0</blendMode>
  <featureBlendMode>0</featureBlendMode>
  <layerOpacity>1</layerOpacity>
  <SingleCategoryDiagramRenderer diagramType="Histogram" attributeLegend="1">
    <DiagramCategory labelPlacementMethod="XHeight" sizeScale="3x:0,0,0,0,0,0" backgroundAlpha="255" minScaleDenominator="0" height="15" opacity="1" backgroundColor="#ffffff" penColor="#000000" scaleBasedVisibility="0" sizeType="MM" diagramOrientation="Up" lineSizeScale="3x:0,0,0,0,0,0" penWidth="0" penAlpha="255" barWidth="5" width="15" lineSizeType="MM" scaleDependency="Area" minimumSize="0" rotationOffset="270" maxScaleDenominator="1e+08" enabled="0">
      <fontProperties description="MS Shell Dlg 2,8.25,-1,5,50,0,0,0,0,0" style=""/>
      <attribute field="" color="#000000" label=""/>
    </DiagramCategory>
  </SingleCategoryDiagramRenderer>
  <DiagramLayerSettings showAll="1" obstacle="0" dist="0" placement="0" linePlacementFlags="18" zIndex="0" priority="0">
    <properties>
      <Option type="Map">
        <Option value="" type="QString" name="name"/>
        <Option name="properties"/>
        <Option value="collection" type="QString" name="type"/>
      </Option>
    </properties>
  </DiagramLayerSettings>
  <geometryOptions geometryPrecision="0" removeDuplicateNodes="0">
    <activeChecks/>
    <checkConfiguration/>
  </geometryOptions>
  <fieldConfiguration>
    <field name="temp_time">
      <editWidget type="TextEdit">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="ts">
      <editWidget type="TextEdit">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="time">
      <editWidget type="Range">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="id">
      <editWidget type="Range">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="posID">
      <editWidget type="TextEdit">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="X_raw">
      <editWidget type="TextEdit">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="Y_raw">
      <editWidget type="TextEdit">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="NBS">
      <editWidget type="TextEdit">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="VARX">
      <editWidget type="TextEdit">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="VARY">
      <editWidget type="TextEdit">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="COVXY">
      <editWidget type="TextEdit">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="x">
      <editWidget type="TextEdit">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="y">
      <editWidget type="TextEdit">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="SD">
      <editWidget type="TextEdit">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="tide_number">
      <editWidget type="Range">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="tidaltime">
      <editWidget type="TextEdit">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="waterlevel">
      <editWidget type="Range">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
  </fieldConfiguration>
  <aliases>
    <alias field="temp_time" index="0" name=""/>
    <alias field="ts" index="1" name=""/>
    <alias field="time" index="2" name=""/>
    <alias field="id" index="3" name=""/>
    <alias field="posID" index="4" name=""/>
    <alias field="X_raw" index="5" name=""/>
    <alias field="Y_raw" index="6" name=""/>
    <alias field="NBS" index="7" name=""/>
    <alias field="VARX" index="8" name=""/>
    <alias field="VARY" index="9" name=""/>
    <alias field="COVXY" index="10" name=""/>
    <alias field="x" index="11" name=""/>
    <alias field="y" index="12" name=""/>
    <alias field="SD" index="13" name=""/>
    <alias field="tide_number" index="14" name=""/>
    <alias field="tidaltime" index="15" name=""/>
    <alias field="waterlevel" index="16" name=""/>
  </aliases>
  <excludeAttributesWMS/>
  <excludeAttributesWFS/>
  <defaults>
    <default field="temp_time" expression="" applyOnUpdate="0"/>
    <default field="ts" expression="" applyOnUpdate="0"/>
    <default field="time" expression="" applyOnUpdate="0"/>
    <default field="id" expression="" applyOnUpdate="0"/>
    <default field="posID" expression="" applyOnUpdate="0"/>
    <default field="X_raw" expression="" applyOnUpdate="0"/>
    <default field="Y_raw" expression="" applyOnUpdate="0"/>
    <default field="NBS" expression="" applyOnUpdate="0"/>
    <default field="VARX" expression="" applyOnUpdate="0"/>
    <default field="VARY" expression="" applyOnUpdate="0"/>
    <default field="COVXY" expression="" applyOnUpdate="0"/>
    <default field="x" expression="" applyOnUpdate="0"/>
    <default field="y" expression="" applyOnUpdate="0"/>
    <default field="SD" expression="" applyOnUpdate="0"/>
    <default field="tide_number" expression="" applyOnUpdate="0"/>
    <default field="tidaltime" expression="" applyOnUpdate="0"/>
    <default field="waterlevel" expression="" applyOnUpdate="0"/>
  </defaults>
  <constraints>
    <constraint field="temp_time" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="ts" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="time" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="id" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="posID" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="X_raw" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="Y_raw" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="NBS" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="VARX" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="VARY" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="COVXY" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="x" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="y" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="SD" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="tide_number" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="tidaltime" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="waterlevel" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
  </constraints>
  <constraintExpressions>
    <constraint desc="" field="temp_time" exp=""/>
    <constraint desc="" field="ts" exp=""/>
    <constraint desc="" field="time" exp=""/>
    <constraint desc="" field="id" exp=""/>
    <constraint desc="" field="posID" exp=""/>
    <constraint desc="" field="X_raw" exp=""/>
    <constraint desc="" field="Y_raw" exp=""/>
    <constraint desc="" field="NBS" exp=""/>
    <constraint desc="" field="VARX" exp=""/>
    <constraint desc="" field="VARY" exp=""/>
    <constraint desc="" field="COVXY" exp=""/>
    <constraint desc="" field="x" exp=""/>
    <constraint desc="" field="y" exp=""/>
    <constraint desc="" field="SD" exp=""/>
    <constraint desc="" field="tide_number" exp=""/>
    <constraint desc="" field="tidaltime" exp=""/>
    <constraint desc="" field="waterlevel" exp=""/>
  </constraintExpressions>
  <expressionfields/>
  <attributeactions>
    <defaultAction value="{00000000-0000-0000-0000-000000000000}" key="Canvas"/>
  </attributeactions>
  <attributetableconfig actionWidgetStyle="dropDown" sortOrder="0" sortExpression="">
    <columns>
      <column hidden="0" type="field" width="-1" name="temp_time"/>
      <column hidden="0" type="field" width="-1" name="ts"/>
      <column hidden="0" type="field" width="-1" name="time"/>
      <column hidden="0" type="field" width="-1" name="id"/>
      <column hidden="0" type="field" width="-1" name="posID"/>
      <column hidden="0" type="field" width="-1" name="X_raw"/>
      <column hidden="0" type="field" width="-1" name="Y_raw"/>
      <column hidden="0" type="field" width="-1" name="NBS"/>
      <column hidden="0" type="field" width="-1" name="VARX"/>
      <column hidden="0" type="field" width="-1" name="VARY"/>
      <column hidden="0" type="field" width="-1" name="COVXY"/>
      <column hidden="0" type="field" width="-1" name="x"/>
      <column hidden="0" type="field" width="-1" name="y"/>
      <column hidden="0" type="field" width="-1" name="SD"/>
      <column hidden="0" type="field" width="-1" name="tide_number"/>
      <column hidden="0" type="field" width="-1" name="tidaltime"/>
      <column hidden="0" type="field" width="-1" name="waterlevel"/>
      <column hidden="1" type="actions" width="-1"/>
    </columns>
  </attributetableconfig>
  <conditionalstyles>
    <rowstyles/>
    <fieldstyles/>
  </conditionalstyles>
  <editform tolerant="1"></editform>
  <editforminit/>
  <editforminitcodesource>0</editforminitcodesource>
  <editforminitfilepath></editforminitfilepath>
  <editforminitcode><![CDATA[# -*- coding: utf-8 -*-
"""
QGIS forms can have a Python function that is called when the form is
opened.

Use this function to add extra logic to your forms.

Enter the name of the function in the "Python Init function"
field.
An example follows:
"""
from qgis.PyQt.QtWidgets import QWidget

def my_form_open(dialog, layer, feature):
	geom = feature.geometry()
	control = dialog.findChild(QWidget, "MyLineEdit")
]]></editforminitcode>
  <featformsuppress>0</featformsuppress>
  <editorlayout>generatedlayout</editorlayout>
  <editable>
    <field name="COVXY" editable="1"/>
    <field name="NBS" editable="1"/>
    <field name="SD" editable="1"/>
    <field name="VARX" editable="1"/>
    <field name="VARY" editable="1"/>
    <field name="X_raw" editable="1"/>
    <field name="Y_raw" editable="1"/>
    <field name="id" editable="1"/>
    <field name="posID" editable="1"/>
    <field name="temp_time" editable="1"/>
    <field name="tidaltime" editable="1"/>
    <field name="tide_number" editable="1"/>
    <field name="time" editable="1"/>
    <field name="ts" editable="1"/>
    <field name="waterlevel" editable="1"/>
    <field name="x" editable="1"/>
    <field name="y" editable="1"/>
  </editable>
  <labelOnTop>
    <field labelOnTop="0" name="COVXY"/>
    <field labelOnTop="0" name="NBS"/>
    <field labelOnTop="0" name="SD"/>
    <field labelOnTop="0" name="VARX"/>
    <field labelOnTop="0" name="VARY"/>
    <field labelOnTop="0" name="X_raw"/>
    <field labelOnTop="0" name="Y_raw"/>
    <field labelOnTop="0" name="id"/>
    <field labelOnTop="0" name="posID"/>
    <field labelOnTop="0" name="temp_time"/>
    <field labelOnTop="0" name="tidaltime"/>
    <field labelOnTop="0" name="tide_number"/>
    <field labelOnTop="0" name="time"/>
    <field labelOnTop="0" name="ts"/>
    <field labelOnTop="0" name="waterlevel"/>
    <field labelOnTop="0" name="x"/>
    <field labelOnTop="0" name="y"/>
  </labelOnTop>
  <widgets/>
  <previewExpression>id</previewExpression>
  <mapTip></mapTip>
  <layerGeometryType>0</layerGeometryType>
</qgis>
